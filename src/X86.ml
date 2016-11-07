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
(*let first_free = 3
let rff = R first_free*)
let word_size = 4

let eax = R 0
let ebx = R 1
let ecx = R 2
let edx = R 3
let esi = R 4
let edi = R 5
            
type instr =
  | X86Binop of string * opnd * opnd (* add,+ / sub,- / mul,/ / cmp,= / xor,@ / mov,-> + opnd + opnd *)
  | X86Div   of opnd
  | X86Push  of opnd
  | X86Pop   of opnd
  | X86Cdq
  | X86Set   of string * string (* <=/</==/!=/>=/> + le *)
  | X86Call  of string
  | X86Lbl   of string
  | X86Jmp   of string (* lbl *)
  | X86CJmp  of string * string (* z/nz + lbl *)
  | X86Enter of int
  | X86Leave
  | X86Ret
             
module M = Map.Make (String)
                    
class x86env =
object(self)
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
    match (self#get_local_option x) with
    | Some y -> y
    | None -> allocated_local := !allocated_local + 1;
              local_vars := M.add x (M (!allocated_local - 1)) !local_vars;
              M (!allocated_local - 1)
  (* STACK *)
  val allocated_stack = ref 0
  method allocated_stack = !allocated_stack
  method update_stack n = allocated_stack := max !allocated_stack n
  method allocate stack =
    match stack with
    | [] -> self#update_stack 1; S 0
    | (S n)::_ -> self#update_stack (n + 2); S (n + 1)
  (* TOTAL *)
  method allocated_total = self#allocated_local + self#allocated_stack
  (* CLEAR *)
  method clear = local_vars := M.empty; allocated_local := 0; allocated_stack := 0
end

(*let allocate env stack =
  match stack with
  | []                                -> rff
  | (S n)::_                          -> env#allocate (n+1); S (n+1)
  | (R n)::_ when n < num_of_regs - 1 -> R (n+1)
  | _                                 -> env#allocate (0); S 0*)

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
      | X86Enter n         -> Printf.sprintf "\tpushl\t%%ebp\n\tmovl\t%%esp,\t%%ebp\n\tsubl\t$%d,\t%%esp" (n * word_size)
      | X86Leave           -> Printf.sprintf "\tleave"
      | X86Ret             -> "\tret"
                                          
  end

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
                (s::stack, [X86Call "read"; X86Binop ("->", eax, s)])
             | S_WRITE ->
                let s::stack' = stack
                in
                (stack', [X86Push s; X86Call "write"; X86Pop s])
             | S_PUSH n ->
                let s = env#allocate stack
                in
                (s::stack, [X86Binop ("->", L n, s)])
             | S_LD x ->
                let s = env#allocate stack
                in
                let m = env#get_local x
                in                    
                (s::stack, [X86Binop ("->", m, eax); X86Binop ("->", eax, s)])
             | S_ST x ->
                let s::stack' = stack
                in
                let m = env#create_local x
                in
                (stack', [X86Binop ("->", s, eax); X86Binop ("->", eax, m)])
             | S_BINOP o ->
                (let l::r::stack' = stack
                 in
                 let rec ifnreg (x,y) = match x,y with
                   | R _, _ | _, R _ -> ([], x)
                   | _ -> ([X86Binop ("->", x, ebx)], ebx)
                 in
                 let cmd (x,y) = function
                   | "+" | "-" -> [X86Binop (o, x, y)]
                   | "*" -> (match x,y with | _, R _ -> [X86Binop (o, x, y)] | _ -> [X86Binop (o, y, x); X86Binop ("->", x, y)])
                   | op -> [X86Binop ("->", L 0, eax); X86Binop ("=", x, y); X86Set (o, "al"); X86Binop ("->", eax, y)]
                 in
                 let cdqo = function
                   | "/" -> eax
                   | "%" -> edx
                 in
                 match o with
                 | "/" | "%" -> (r::stack', [X86Binop ("->", r, eax); X86Cdq; X86Div l; X86Binop ("->", cdqo o, r)])
                 | _ -> let (prereq, l') = ifnreg (l, r)
                        in
                        (r::stack', prereq @ cmd (l', r) o))
             | S_LBL l -> (stack, [X86Lbl l])
             | S_JMP l -> (stack, [X86Jmp l])
             | S_CJMP (c, l) ->
                let s::stack' = stack
                in
                (stack', [X86Binop ("->", s, eax); X86Binop ("=", L 0, eax); X86CJmp (c, l)])
             | S_CALL (name, args) ->
                let rec process num stack =
                  match num with
                  | 0 ->
                     let s = env#allocate stack
                     in
                     (s::stack, [], [X86Binop ("->", eax, s)])
                  | n ->
                     let s::stack' = stack
                     in
                     let st, bn, ed = process (num - 1) stack'
                     in
                     (st, (X86Push s)::bn, (X86Pop eax)::ed)
                in
                let st, bn, ed = process (List.length args) stack
                in
                (st, bn @ [X86Call name] @ (List.rev ed))
             | S_RET ->
                let s::stack' = stack
                in
                (stack', [X86Binop ("->", s, eax)])
           in
	   x86code @ (compile stack' code')
      in
      compile [] code
  end
    
let compile_fdef env (name, args, s_body) =
  env#clear;
  let n = List.length args
  in
  List.iteri (fun ind arg -> env#set_local arg @@ F (n - ind - 1)) args;
  [X86Lbl name]
  @ [X86Enter env#allocated_total]
  @ (Compile.stack_program env s_body)
  @ [X86Leave]
  @ [X86Ret]
      
let compile_main env s_main =
  env#clear;
  [X86Lbl "main"]
  @ [X86Enter env#allocated_total]
  @ (Compile.stack_program env s_main)
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
  let c_fdefs = List.fold_left
                  (fun c_fdefs s_fdef -> c_fdefs @ (compile_fdef env s_fdef))
                  []
                  s_fdefs
  in
  let c_main = compile_main env s_main
  in
  !"\t.text";
  !"\t.globl\tmain";
  List.iter (fun i -> !(Show.instr env i)) (c_fdefs @ c_main);
  !!"\n";
  Buffer.contents asm
                  
(*let compile prog =
  let env = new x86env in
  let code = Compile.stack_program env @@ (*StackMachine.Compile.prog prog*) [S_LBL "main"] in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  !"\t.text";
  List.iter (fun x ->
      !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size))
            env#local_vars;
  !"\t.globl\tmain";
  let prologue, epilogue =
    if env#allocated = 0
    then (fun () -> ()), (fun () -> ())
    else
      (fun () ->
        !"\tpushl\t%ebp";
        !"\tmovl\t%esp,\t%ebp";
        !(Printf.sprintf "\tsubl\t$%d,\t%%esp" (env#allocated * word_size))
      ),
      (fun () ->
        !"\tmovl\t%ebp,\t%esp";
        !"\tpopl\t%ebp"
      )
  in
  !"main:";
  prologue();
  List.iter (fun i -> !(Show.instr i)) code;
  epilogue();
  !"\txorl\t%eax,\t%eax";
  !"\tret";
  Buffer.contents asm*)
                  
let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name)
  in
  Printf.fprintf outf "%s" (compile stmt);
  close_out outf;
  let runtime_o = (try Sys.getenv "RUNTIME_O" with | Not_found -> failwith "Please, provide a runtime.o file!")
  in
  let gcc_flags = "" (*"-Ofast"*)
  in
  ignore (Sys.command (Printf.sprintf "gcc %s -m32 -o %s %s %s.s" gcc_flags name runtime_o name))
