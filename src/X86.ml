type opnd =
  | R of int (* register, 0.. *)
  | F of int (* funcs args, 0.. *)
  | M of int (* local var with identifier, 0.. *)
  | S of int (* stack, 0.. *)
  | L of int (* const, int *)
let x86regs = [|
    "%eax";
    "%ebx";
    "%ecx";
    "%edx";
    "%esi";
    "%edi"
  |]
let num_of_regs = Array.length x86regs
let word_size = 4
let eax = R 0
let ebx = R 1
let ecx = R 2
let edx = R 3
let esi = R 4
let edi = R 5
type instr =
  | X86Binop    of string * opnd * opnd (* add,+ / sub,- / mul,/ / cmp,= / xor,@ / mov,-> + opnd + opnd *)
  | X86Div      of opnd
  | X86Push     of opnd
  | X86Pop      of opnd
  | X86Cdq
  | X86Set      of string * string (* <=/</==/!=/>=/> + le *)
  | X86Call     of string
  | X86Lbl      of string
  | X86Jmp      of string (* lbl *)
  | X86CJmp     of string * string (* z/nz + lbl *)
  | X86Enter
  | X86Leave
  | X86Ret
  | X86Allocate of int
module S =
  Set.Make
    (struct
      type t = int
      let compare = compare
    end)
module M = Map.Make (String)          
class x86env =
object(self)
  (* REGISTERS *)
  val regs_nums = ref S.empty
  method private clear_regs_nums =
    regs_nums := S.empty;
    for i = 1 to num_of_regs - 1 do
      regs_nums := S.add i !regs_nums
    done
  method regs_nums = S.cardinal !regs_nums
  val max_regs_used = ref 0
  method max_regs_used = !max_regs_used
  method private get_local_reg_num_option =
    try
      let elem = S.min_elt !regs_nums
      in
      max_regs_used := max !max_regs_used elem;
      regs_nums := S.remove elem !regs_nums;
      Some elem
    with Not_found -> None
  method private get_local_reg_option =
    match self#get_local_reg_num_option with
    | Some n -> Some (R n)
    | None -> None
  (* LOCAL *)
  val local_vars = ref M.empty
  val allocated_local = ref 0
  method allocated_local = !allocated_local
  method set_local x opnd = local_vars := M.add x opnd !local_vars
  method get_local x = M.find x !local_vars
  method private get_local_option x =
    try
      Some (M.find x !local_vars)
    with Not_found -> None
  method create_local x =
    match self#get_local_option x with
    | Some y -> y
    | None ->
       match self#get_local_reg_option with
       | Some y -> self#set_local x y; y
       | None ->
          allocated_local := !allocated_local + 1;
          self#set_local x @@ M (!allocated_local - 1);
          M (!allocated_local - 1)
  (* STACK *)
  val allocated_stack_max = ref 0
  method allocated_stack_max = !allocated_stack_max
  method private update_stack n = allocated_stack_max := max !allocated_stack_max n
  method allocate stack =
    match self#get_local_reg_option with
    | Some r -> r
    | None ->
       match stack with
       | [] | (R _)::_ -> S 0
       | (S n)::_ -> S (n + 1)
  method push stack opnd =
    (match opnd with
     | R n -> regs_nums := S.remove n !regs_nums
     | S n -> self#update_stack (n + 1));
    opnd::stack
  method pop stack =
    let s::stack' = stack
    in
    (match s with
     | R n -> regs_nums := S.add n !regs_nums
     | _ -> ());
    s, stack'
  (* TOTAL *)
  method allocated_total = self#allocated_local + self#allocated_stack_max
  (* CLEAR *)
  method clear = self#clear_regs_nums; local_vars := M.empty; allocated_local := 0; allocated_stack_max := 0
end
module Show =
  struct
    let opnd env = function
      | R i -> x86regs.(i)
      | F i -> Printf.sprintf "%d(%%ebp)" ((i + 2) * word_size)              
      | M i -> Printf.sprintf "-%d(%%ebp)" ((i + 1) * word_size)
      | S i -> Printf.sprintf "-%d(%%ebp)" ((env#allocated_local + i + 1) * word_size)
      | L i -> Printf.sprintf "$%d" i                      
    let binop_to_x86 = function
      | "+"  -> "addl"
      | "-"  -> "subl"
      | "*"  -> "imull"
      | "="  -> "cmpl"
      | "->" -> "movl"
      | "@"  -> "xorl"
    let set_to_x86 = function
      | "<=" -> "le"
      | "<"  -> "l"
      | "==" -> "e"
      | "!=" -> "ne"
      | ">=" -> "ge"
      | ">"  -> "g"
    let instr env =
      let cnvt x = opnd env x
      in
      function
      | X86Binop (o, x, y) -> Printf.sprintf "\t%s\t%s,\t%s" (binop_to_x86 o) (cnvt x) (cnvt y)
      | X86Div x           -> Printf.sprintf "\tidivl\t%s" (cnvt x)
      | X86Push x          -> Printf.sprintf "\tpushl\t%s" (cnvt x)
      | X86Pop x           -> Printf.sprintf "\tpopl\t%s" (cnvt x)
      | X86Cdq             -> "\tcdq"
      | X86Set (o, s)      -> Printf.sprintf "\tset%s\t%%%s" (set_to_x86 o) s
      | X86Call s          -> Printf.sprintf "\tcall\t%s" s
      | X86Lbl s           -> Printf.sprintf "%s:" s
      | X86Jmp s           -> Printf.sprintf "\tjmp\t%s" s
      | X86CJmp (s, l)     -> Printf.sprintf "\tj%s\t%s" s l
      | X86Enter           -> Printf.sprintf "\tpushl\t%%ebp\n\tmovl\t%%esp,\t%%ebp"
      | X86Leave           -> Printf.sprintf "\tleave"
      | X86Ret             -> "\tret"
      | X86Allocate n      -> Printf.sprintf "\tsubl\t$%d,\t%%esp" (n * word_size)                            
  end
(* СЕЙЧАС *)
(* наш стек не повторяет реальный стек, мы считаем на сколько максимально мы можем расшириться - easy mode (?) *)
(* используем регистры начиная с ebx - это надо учитывать, a в eax возврат функции *)
(* регистры которые мы используем в процессе исполнения команд - надо сохранять и восстанавливать *)
module Compile =
  struct
    open StackMachine.Instrs
    let stack_program env code =
      let rec compile stack code =
	match code with
	| [] | (S_END)::_ -> []
	| i::code' ->
	   let (stack', x86code) =
             match i with
             | S_READ ->
                let s = env#allocate stack
                in
                let pre, post =
                  if s = ecx
                  then [X86Push edx], [X86Pop edx]
                  else
                    if s = edx
                    then [X86Push ecx], [X86Pop ecx]
                    else [X86Push ecx; X86Push edx], [X86Pop edx; X86Pop ecx]
                in (* because scanf modify ecx and edx *)
                (env#push stack s, pre @ [X86Call "read"; X86Binop ("->", eax, s)] @ post)
             | S_WRITE ->
                let s, stack' = env#pop stack
                in
                let pre, post =
                  if s = ecx
                  then [X86Push edx], [X86Pop edx]
                  else
                    if s = edx
                    then [X86Push ecx], [X86Pop ecx]
                    else [X86Push ecx; X86Push edx], [X86Pop edx; X86Pop ecx]
                in (* because printf modify ecx and edx *)
                (stack', pre @ [X86Push s; X86Call "write"; X86Pop s] @ post)
             | S_PUSH n ->
                let s = env#allocate stack
                in
                (env#push stack s, [X86Binop ("->", L n, s)])
             | S_LD x ->
                let m = env#get_local x
                in
                let s = env#allocate stack
                in
                (env#push stack s,
                 match s, m with
                 | R _, _ | _, R _ -> [X86Binop ("->", m, s)]
                 | _ -> [X86Binop ("->", m, eax); X86Binop ("->", eax, s)])
             | S_ST x ->
                let m = env#create_local x
                in
                let s, stack' = env#pop stack
                in
                (stack',
                 match s, m with
                 | R _, _ | _, R _ -> [X86Binop ("->", s, m)]
                 | _ -> [X86Binop ("->", s, eax); X86Binop ("->", eax, m)])
             | S_BINOP o ->
                let l, stack'' = env#pop stack
                in
                let r, stack' = env#pop stack''
                in
                let cmds =
                  match o with
                  | "+" | "-" | "*" ->
                     let pre, post, l', r' =
                       match l, r with
                       | R _, _ | _, R _ -> [], [], l, r
                       | _ -> [X86Binop ("->", r, eax)], [X86Binop ("->", eax, r)], l, eax
                     in
                     pre @ [X86Binop (o, l', r')] @ post
                  | "/" | "%" ->
                     let mr o =
                       match o with
                       | "/" -> eax
                       | "%" -> edx
                     in
                     let pre, post =
                       if r = edx
                       then
                         if o = "/"
                         then [], [X86Binop ("->", eax, r)]
                         else [], []
                       else
                         [X86Push edx], [X86Binop ("->", mr o, r); X86Pop edx]
                     in
                     pre @ [X86Binop ("->", r, eax); X86Cdq; X86Div l] @ post
                  | _ ->
                     let pre, post, l', r' =
                       match l, r with
                       | R _, _ | _, R _ -> [], [], l, r
                       | _ -> [X86Push ebx; X86Binop ("->", r, ebx)], [X86Pop ebx], l, ebx
                     in
                     pre @ [X86Binop ("@", eax, eax); X86Binop ("=", l', r'); X86Set (o, "al"); X86Binop ("->", eax, r)] @ post
                in
                (env#push stack' r, cmds)
             | S_LBL l -> (stack, [X86Lbl l])
             | S_JMP l -> (stack, [X86Jmp l])
             | S_CJMP (c, l) ->
                let s, stack' = env#pop stack
                in
                (stack', [X86Binop ("->", s, eax); X86Binop ("=", L 0, eax); X86CJmp (c, l)])
             | S_CALL (name, args) ->
                let rec process num stack =
                  match num with
                  | 0 ->
                     let s = env#allocate stack
                     in
                     (env#push stack s, [], [X86Binop ("->", eax, s)])
                  | n ->
                     let s, stack' = env#pop stack
                     in
                     let st, bn, ed = process (num - 1) stack'
                     in
                     (st, (X86Push s)::bn, (X86Pop eax)::ed)
                in
                let st, bn, ed = process (List.length args) stack
                in
                (st, bn @ [X86Call name] @ (List.rev ed))
             | S_RET ->
                let s, stack' = env#pop stack
                in
                (stack', [X86Binop ("->", s, eax)])
           in
	   x86code @ (compile stack' code')
      in
      compile [] code
  end
let rec regs_to_stack n =
  match n with
  | 0 -> [], []
  | _ ->
     let pre, post = regs_to_stack (n - 1)
     in
     pre @ [X86Push (R n)], [X86Pop (R n)] @ post                                          
let compile_fdef env (name, args, s_body) =
  env#clear;
  List.iteri (fun ind arg -> env#set_local arg @@ F ind) @@ List.rev args;
  let code = Compile.stack_program env s_body
  in
  let prer, postr = regs_to_stack env#max_regs_used
  in
  [X86Lbl name]
  @ [X86Enter]
  @ [X86Allocate env#allocated_total]
  @ prer
  @ code
  @ postr
  @ [X86Leave]
  @ [X86Ret]  
let compile_main env s_main =
  env#clear;
  let code = Compile.stack_program env s_main
  in
  let prer, postr = regs_to_stack env#max_regs_used
  in
  [X86Lbl "main"]
  @ [X86Enter]
  @ [X86Allocate env#allocated_total]
  @ prer
  @ code
  @ postr
  @ [X86Binop ("@", eax, eax)]
  @ [X86Leave]
  @ [X86Ret]
let compile prog =
  let env = new x86env
  in
  let (s_fdefs, s_main) = StackMachine.Compile.prog prog
  in
  let asm = Buffer.create 1024
  in
  let (!!) s = Buffer.add_string asm s
  in
  let (!) s = !!s; !!"\n"
  in
  let add_asm list = List.iter (fun i -> !(Show.instr env i)) list
  in
  !"\t.text";
  !"\t.globl\tmain";
  List.iter (fun s_fdef -> add_asm @@ compile_fdef env s_fdef) s_fdefs;
  add_asm @@ compile_main env s_main;
  !!"\n";
  Buffer.contents asm      
let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name)
  in
  Printf.fprintf outf "%s" (compile stmt);
  close_out outf;
  let runtime_o = (try Sys.getenv "RUNTIME_O" with | Not_found -> failwith "Please, provide a runtime.o file!")
  in
  let gcc_flags = "-g -Ofast"
  in
  ignore (Sys.command (Printf.sprintf "gcc %s -m32 -o %s %s %s.s" gcc_flags name runtime_o name))
