type opnd =
  | R8 of int (* register (8 bit), 0.. *)
  | R of int (* register (32 bit), 0.. *)
  | F of int (* funcs args, 0.. *)
  | M of int (* local var with identifier, 0.. *)
  | S of int (* stack, 0.. *)
  | L of int (* const, int *)
  | D of string (* data, 0.. *)
  | AR (* array cell using eax and ecx *)
  | TT of int (* 0 for primitive, 1 for ptr *)

(* EAX, ECX, EDX *)
(* eax - return value of func *)
(* ecx - type of return of func *)
(* eax, ecx, edx - for free use *)
(* ESP, EBP *)
(* esp - stack pointer *)
(* ebp - for save stack frame *)
(* EBX, ESI, EDI *)
(* ebx - do not use so far *)
(* esi, edi - needs to be saved and restore *)
let regs32 = [|"%eax"; "%ecx"; "%edx"; "%ebx"; "%esp"; "%ebp"; "%esi"; "%edi"|]
let eax, ecx, edx, ebx, esp, ebp, esi, edi = R 0, R 1, R 2, R 3, R 4, R 5, R 6, R 7
            
let regs8 = [| "%ah"; "%al"; "%ch"; "%cl"; "%dh"; "%dl"; "%bh"; "%bl"|]          
let ah, al, ch, cl, dh, dl, bh, bl = R8 0, R8 1, R8 2, R8 3, R8 4, R8 5, R8 6, R8 7
                            
let word_size = 4
                  
type instr =
  | X86Binop of string * opnd * opnd
  | X86Div of opnd
  | X86Push of opnd
  | X86Pop of opnd
  | X86Cdq
  | X86Set of string * opnd
  | X86Call of string
  | X86Lbl of string
  | X86Jmp of string
  | X86CJmp of string * string
  | X86Enter
  | X86Leave
  | X86Ret
  | X86Allocate of int
  | X86Free of int

module S =
  BatSet.Make
    (struct
      type t = opnd
      let compare x y =
        match x, y with
        | R n, R m -> BatInt.compare n m
        | _ -> 0
    end)
module M = BatMap.Make(String)
let saved_regs = S.of_list [esi; edi]
class env = object(self)
  (* REGISTERS *)
  val free_regs : S.t ref = ref saved_regs (* caller free regs *)
  val used_regs : S.t ref = ref S.empty (* set of used regs *)
  method used_regs = S.to_list !used_regs
  method used_regs_num = List.length self#used_regs
  method private get_free_reg_option =
    try Some (S.min_elt !free_regs)
    with Not_found -> None
  (* LOCAL *) (* [type][value] *)
  val local_vars : opnd M.t ref = ref M.empty (* with func args, pointing to value *)
  val allocated_local : int ref = ref 0 (* without func args *)
  method locals = !local_vars
  method allocated_local = !allocated_local
  method set_local x opnd = local_vars := M.add x opnd !local_vars
  method get_local x = M.find x !local_vars
  method private get_local_option x =
    try Some (self#get_local x)
    with Not_found -> None
  method create_local x =
    match self#get_local_option x with
    | Some y -> y
    | None ->
       self#set_local x @@ M !allocated_local;
       allocated_local := !allocated_local + 1;
       M (!allocated_local - 1)
  (* STACK *)
  val stack : opnd list ref = ref [] (* stack *)
  val allocated_stack_max : int ref = ref 0 (* max stack size *)
  method private allocated_stack_max = !allocated_stack_max
  method private update_stack_max n = allocated_stack_max := max !allocated_stack_max n
  method allocate =
    match self#get_free_reg_option with
    | Some r -> r
    | None ->
       match !stack with
       | [] | (R _)::_ -> S 0
       | (S n)::_ -> S (n + 1)
  method push opnd =
    (match opnd with
     | R _ as r -> free_regs := S.remove r !free_regs; used_regs := S.add r !used_regs
     | S n -> self#update_stack_max (n + 1));
    stack := opnd::!stack
  method pop =
    let s::stack' = !stack in
    (match s with
     | R _ as r -> free_regs := S.add r !free_regs;
     | _ -> ());
    stack := stack';
    s
  (* GC WITH TYPES *)
  method allocate_t =
    let m = self#allocate in
    match m with
    | R n -> esi, edi (* R n, R (n + 1) *)
    | S n -> S n, S (n + 1)
  method push_t t v =
    self#push t; self#push v
  method pop_t =
    let v = self#pop in
    let t = self#pop in
    t, v
  method set_local_t x o = self#set_local x o
  method get_local_t x =
    match self#get_local x with
    | M n -> M (n - 1), M n
    | F n -> F (n + 1), F n
  method create_local_t x =
    (match self#get_local_option x with
     | Some y -> ()
     | None ->
        self#set_local_t x @@ M (!allocated_local + 1);
        allocated_local := !allocated_local + 2);
    self#get_local_t x
  (* TOTAL *)
  method allocated_total = self#allocated_local + self#allocated_stack_max
  (* CLEAR *)
  method clear = free_regs := saved_regs; used_regs := S.empty; local_vars := M.empty; allocated_local := 0; allocated_stack_max := 0; stack := []
  (* STRINGS *)
  val strings : string list ref = ref [] (* used strings *)
  method add_string s = strings := s::!strings; D (Printf.sprintf "string%d" @@ List.length !strings - 1)
  method used_strings = List.rev !strings
end

module Compile =
  struct
    open StackMachine.Instrs
    module V = Language.Value
    let stack_program env code =
      (* moving from opnd to opnd with register if needed *)
      let mov_w_reg ?(r=eax) f t =
        match f, t with
        | _, _ when f=t -> []
        | R _, _ | _, R _ -> [X86Binop ("->", f, t)]
        | _ -> [X86Binop ("->", f, r); X86Binop ("->", r, t)]
      in
      (* prologue and epilogue for funcs *)
      let precall n =
        let rec precall' = function
          | 0 ->
             let x = env#allocate in
             env#push x;
             [], [X86Binop ("->", eax, x)]
          | n ->
             let x = env#pop in
             let pre, post = precall' (n - 1) in
             (X86Push x)::pre, post
        in
        let pre, post = precall' n in
        pre, (X86Free n)::post
      in
      (* prologue and epilogur for funcs with gc types *)
      let precall_t n =
        let rec precall' = function
          | 0 ->
             let t, v = env#allocate_t in
             env#push_t t v;
             [], [X86Binop ("->", ecx, t); X86Binop ("->", eax, v)]
          | n ->
             let t, v = env#pop_t in
             let pre, post = precall' (n - 1) in
             [X86Push t; X86Push v] @ pre, post
        in
        let pre, post = precall' n in
        pre, (X86Free (2 * n))::post
      in
      let precall_t_b n =
        let rec precall' = function
          | 0 ->
             let t, v = env#allocate_t in
             env#push_t t v;
             [], [X86Binop ("->", ecx, t); X86Binop ("->", eax, v)]
          | n ->
             let t, v = env#pop_t in
             let pre, post = precall' (n - 1) in
             (X86Push v)::pre, post
        in
        let pre, post = precall' n in
        pre, (X86Free n)::post
      in
      let rec compile = function
	| [] | (S_END)::_ -> []
	| i::code' ->
	   let x86code =
             match i with
             (*| S_PUSH n ->
                let p = env#allocate in
                env#push p;
                (match n with
                 | V.Int i -> [X86Binop ("->", L i, p)]
                 | V.String s -> [X86Binop ("->", env#add_string s, p)])*)
             | S_PUSH n ->
                let t, v = env#allocate_t in
                env#push_t t v;
                (match n with
                 | V.Int i -> [X86Binop ("->", L 0, t); X86Binop ("->", L i, v)]
                 | V.String s -> [X86Binop ("->", L 1, t); X86Binop ("->", env#add_string s, v)])
             (*| S_POP -> env#pop; []*)
             | S_POP -> env#pop_t; []
             (*| S_LD x ->
                let m = env#get_local x in
                let s = env#allocate in
                env#push s;
                mov_w_reg m s*)
             | S_LD x ->
                let t, v = env#get_local_t x in
                let at, av = env#allocate_t in
                env#push_t at av;
                mov_w_reg t at @ mov_w_reg v av
             (*| S_ST x ->
                let s = env#pop in
                let m = env#create_local x in
                if s = m then [] else mov_w_reg s m*)
             | S_ST x ->
                let t, v = env#pop_t in
                let lt, lv = env#create_local_t x in
                List.concat [mov_w_reg t lt; mov_w_reg v lv;
                             [X86Push v; X86Push t; X86Call "Tgc_inc_ref"; X86Free 2]]
             (*| S_BINOP o ->
                let l = env#pop in
                let r = env#pop in
                let cmds =
                  match o with
                  | "+" | "-" | "*" ->
                     let pre, post, l', r' =
                       match l, r with
                       | R _, _ | _, R _ -> [], [], l, r
                       | _ -> [X86Binop ("->", r, eax)], [X86Binop ("->", eax, r)], l, eax
                     in
                     pre @ [X86Binop (o, l', r')] @ post
                  | "/" | "%" -> [X86Binop ("->", r, eax); X86Cdq; X86Div l; X86Binop ("->", (if o = "/" then eax else edx), r)]
                  | _ ->
                     let pre, post, l', r' =
                       match l, r with
                       | R _, _ | _, R _ -> [], [], l, r
                       | _ -> [X86Binop ("->", r, ecx)], [], l, ecx
                     in
                     pre @ [X86Binop ("@", eax, eax); X86Binop ("=", l', r'); X86Set (o, al); X86Binop ("->", eax, r)] @ post
                in
                env#push r;
                cmds*)
             | S_BINOP o ->
                let t, l = env#pop_t in
                let t, r = env#pop_t in
                let cmds =
                  match o with
                  | "+" | "-" | "*" ->
                     let pre, post, l', r' =
                       match l, r with
                       | R _, _ | _, R _ -> [], [], l, r
                       | _ -> [X86Binop ("->", r, eax)], [X86Binop ("->", eax, r)], l, eax
                     in
                     pre @ [X86Binop (o, l', r')] @ post
                  | "/" | "%" -> [X86Binop ("->", r, eax); X86Cdq; X86Div l; X86Binop ("->", (if o = "/" then eax else edx), r)]
                  | _ ->
                     let pre, post, l', r' =
                       match l, r with
                       | R _, _ | _, R _ -> [], [], l, r
                       | _ -> [X86Binop ("->", r, ecx)], [], l, ecx
                     in
                     pre @ [X86Binop ("@", eax, eax); X86Binop ("=", l', r'); X86Set (o, al); X86Binop ("->", eax, r)] @ post
                in
                env#push_t t r;
                cmds
             | S_LBL l -> [X86Lbl l]
             | S_JMP l -> [X86Jmp l]
             | S_CJMP (c, l) ->
                let s = env#pop in
                [X86Binop ("->", s, eax); X86Binop ("=", L 0, eax); X86CJmp (c, l)]
             (*| S_CALL (name, args) -> 
                let pre, post = precall (List.length args) in
                List.concat [pre; [X86Call name]; post]*)
             | S_CALL (name, args) ->
                let pre, post = precall_t (List.length args) in
                List.concat [pre; [X86Call name]; post]
             (*| S_BUILTIN (name, argsn) ->
                let pre, post = precall argsn in
                List.concat [pre; [X86Call ("L"^name)]; post]*)
             | S_BUILTIN (name, argsn) ->
                let pre, post = precall_t_b argsn in
                List.concat [pre; [X86Call ("L"^name)]; post]
             (* S_RET ->
                let s = env#pop in
                [X86Binop ("->", s, eax)]*)
             | S_RET ->
                let dec_ref_args =
                  M.fold
                    (fun arg _ l ->
                      let t, v = env#get_local_t arg in
                      [X86Push v; X86Push t; X86Call "Tgc_dec_ref"; X86Free 2] @ l)
                    env#locals
                    []
                in
                let t, v = env#pop_t in
                dec_ref_args @ [X86Call "Tgc_collect"] @ [X86Binop ("->", t, ecx); X86Binop ("->", v, eax)]
             | S_ARRAY (b, n) ->
                let name = match b with Unboxed -> "arrmake" | _ -> "Arrmake" in
                let t, v = env#allocate_t in
                env#push_t t v;
                [X86Push (L 0); X86Push (L n); X86Call ("L"^name); X86Free 2; X86Binop ("->", ecx, t); X86Binop ("->", eax, v)]
             | S_ELEM ->
                let t, i = env#pop_t in
                let t, a = env#pop_t in
                let t, v = env#allocate_t in
                env#push_t t v;
                [X86Push i; X86Push a; X86Call ("Larrget"); X86Free 2; X86Binop ("->", ecx, t); X86Binop ("->", eax, v)]
                (* List.concat [[X86Binop ("->", a, eax); X86Binop ("->", i, ecx)]; mov_w_reg ~r:edx AR v] *)
             | S_STA ->
                let vt, v = env#pop_t in
                let _, i = env#pop_t in
                let at, a = env#pop_t in
                env#push_t at a;
                List.concat [[X86Binop ("->", a, eax); X86Binop ("->", i, ecx)]; mov_w_reg ~r:edx v AR;
                             [X86Push v; X86Push vt; X86Push a; X86Push at; X86Call "Tgc_ref"; X86Free 4]]
           in
	   x86code @ (compile code')
      in
      compile code
  end

module Show =
  struct
    let opnd env = function
      | R8 i -> regs8.(i)
      | R i -> regs32.(i)
      | F i -> Printf.sprintf "%d(%%ebp)" ((i + 2) * word_size)              
      | M i -> Printf.sprintf "-%d(%%ebp)" ((env#used_regs_num + i + 1) * word_size)
      | S i -> Printf.sprintf "-%d(%%ebp)" ((env#used_regs_num + env#allocated_local + i + 1) * word_size)
      | L i -> Printf.sprintf "$%d" i
      | D s -> Printf.sprintf "$%s" s
      | AR -> Printf.sprintf "%d(%%eax,%%ecx,%d)" word_size word_size
      | TT i -> Printf.sprintf "$%d" i
                              
    let binop_to_x86 = function
      | "+" -> "addl"
      | "-" -> "subl"
      | "*" -> "imull"
      | "=" -> "cmpl"
      | "->" -> "movl"
      | "@" -> "xorl"
      | "<->" -> "xchg"
                 
    let set_to_x86 = function
      | "<=" -> "le"
      | "<" -> "l"
      | "==" -> "e"
      | "!=" -> "ne"
      | ">=" -> "ge"
      | ">" -> "g"
                 
    let cjmp_to_x86 = function
      | "==0" -> "z"
      | "!=0" -> "nz"
                   
    let instr env i =
      let opnd x = opnd env x in
      let rec instr' =
        let (!) xs = String.concat "\n" @@ List.map (fun i -> instr' i) xs in
        function
        | X86Binop (o, x, y) -> Printf.sprintf "\t%s\t%s,\t%s" (binop_to_x86 o) (opnd x) (opnd y)
        | X86Div x -> Printf.sprintf "\tidivl\t%s" (opnd x)
        | X86Push x -> Printf.sprintf "\tpushl\t%s" (opnd x)
        | X86Pop x -> Printf.sprintf "\tpopl\t%s" (opnd x)
        | X86Cdq -> "\tcdq"
        | X86Set (o, x) -> Printf.sprintf "\tset%s\t%s" (set_to_x86 o) (opnd x)
        | X86Call l -> Printf.sprintf "\tcall\t%s" l
        | X86Lbl l -> Printf.sprintf "%s:" l
        | X86Jmp l -> Printf.sprintf "\tjmp\t%s" l
        | X86CJmp (c, l) -> Printf.sprintf "\tj%s\t%s" (cjmp_to_x86 c) l
        | X86Enter -> ![X86Push ebp; X86Binop ("->", esp, ebp)]
        | X86Leave -> "\tleave"
        | X86Ret -> "\tret"
        | X86Allocate n -> ![X86Binop ("-", L (n * word_size), esp)]
        | X86Free n -> ![X86Binop ("+", L (n * word_size), esp)]
      in
      instr' i
  end

module Build =
  struct
    let make_inc_ref_args env args =
      List.concat @@
        BatList.mapi
          (fun ind arg ->
            let t, v = env#get_local_t arg in
            [X86Push v; X86Push t; X86Call "Tgc_inc_ref"; X86Free 2])
          args

    (*let make_dec_ref_args env =
      M.fold
        (fun arg _ l ->
          let t, v = env#get_local_t arg in
          [X86Push v; X86Push t; X86Call "Tgc_dec_ref"; X86Free 2] @ l)
        env#locals
        []*)
         
    let regs_to_stack regs = List.map (fun x -> X86Push x) regs, List.map (fun x -> X86Pop x) @@ List.rev regs

    (* OLD *)
    (* [args        ][ptr to back][saved ebp][saved regs  ][allocated mem on stack for func] *)
    (*                                                     [local vars    ][local stack    ] *)
    (* [4 * args num][4          ][4        ][4 * regs num][4 * allocate size              ] *)
    (* NEW *)
    (* [args + types][ptr to back][saved ebp][saved regs  ][allocated mem on stack for func] *)
    (* [[type][arg] ]                                      [local vars    ][local stack    ] *)
    (* [8 * args num][4          ][4        ][4 * regs num][8 * allocate size              ] *)
    let compile_fdef env (name, args, s_body) =
      env#clear;
      List.iteri (fun ind arg -> env#set_local_t arg @@ F (2 * ind)) args;
      let code = Compile.stack_program env s_body in
      let inc_ref_args = make_inc_ref_args env args in
      let push_regs, pop_regs = regs_to_stack env#used_regs in
      List.concat
        [[X86Lbl name];
         [X86Enter];
         inc_ref_args;
         push_regs;
         [X86Allocate env#allocated_total];
         code;
         [X86Free env#allocated_total];
         pop_regs;
         [X86Leave];
         [X86Ret]]
        
    let compile_main env s_main =
      env#clear;
      let code = Compile.stack_program env s_main in
      let push_regs, pop_regs = regs_to_stack env#used_regs in
      List.concat
        [[X86Lbl "main"];
         [X86Enter];
         push_regs;
         [X86Allocate env#allocated_total];
         code;
         [X86Free env#allocated_total];
         pop_regs;
         [X86Binop ("@", eax, eax)];
         [X86Leave];
         [X86Ret]]
        
    let compile prog =
      let env = new env in
      let (s_fdefs, s_main) = StackMachine.Compile.prog prog in
      let asm = Buffer.create 1024 in
      let (!!) s = Buffer.add_string asm s in
      let (!) s = !!s; !!"\n" in
      let add_asm list = List.iter (fun i -> !(Show.instr env i)) list in
      (* EXTERN *)
      (* List.iter (fun f -> !(Printf.sprintf "\t.extern L%s" f)) Stdlib.builtins_list; *)
      (* !!"\n"; *)
      (* TEXT *)
      !"\t.text";
      !"\t.globl\tmain";
      List.iter (fun s_fdef -> add_asm @@ compile_fdef env s_fdef) s_fdefs;
      add_asm @@ compile_main env s_main;
      !!"\n";
      (* DATA *)
      !"\t.data";
      List.iteri (fun i s -> !(Printf.sprintf "string%d:\n\t.int %d\n\t.ascii \"%s\"" i (Bytes.length s) s)) env#used_strings;
      !!"\n";
      Buffer.contents asm
                      
    let run stmt name =
      let outf = open_out @@ Printf.sprintf "%s.s" name in
      Printf.fprintf outf "%s" (compile stmt);
      close_out outf;
      let runtime_src =
        try Sys.getenv "RUNTIME_SRC"
        with Not_found -> failwith "Please, provide a path to runtime src in RUNTIME_SRC env"
      in
      let gcc_flags = "-g -Ofast" in
      let runtime_lib = "runtime" in
      ignore @@ Sys.command (Printf.sprintf "g++ -L %s %s -m32 -o %s %s.s -l%s" runtime_src gcc_flags name name runtime_lib)
  end
