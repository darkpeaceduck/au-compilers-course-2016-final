(* ПОСЛЕ X86 *)
(* //TODO переписать !! and && в x86 для ускорения *)
(* //TODO избавиться от второго аругмента в call на стадии fdefs *)
(* //TODO чистить стэк после вызова функции как процедуры *)
(* //TODO HashMap instead of Map  *)
(* //TODO ??? *)
(* //TODO сделать рефакторинг *)
(* //TODO перепройти все тесты *)
(* //TODO переделать функции в SM и Int правильно? и с классами *)

type opnd = R of int | S of int | M of string | L of int

let x86regs = [|
    "%eax";
    "%ebx";
    "%edx";
    "%ecx";
    "%esi";
    "%edi"
  |]

let num_of_regs = Array.length x86regs
let first_free = 3
let rff = R first_free
let word_size = 3

let eax = R 0
let ebx = R 1
let edx = R 2
let ecx = R 3
let esi = R 4
let edi = R 5
            
type instr =
  | X86Binop of string * opnd * opnd (* add,+ / sub,- / mul,/ / cmp,= / mov,-> + opnd + opnd *)
  | X86Div   of opnd
  | X86Push  of opnd
  | X86Pop   of opnd
  | X86Cdq
  | X86Set   of string * string (* <=/</==/!=/>=/> + le *)
  | X86Call  of string
  | X86Lbl   of string
  | X86Jmp   of string (* lbl *)
  | X86CJmp  of string * string (* z/nz + lbl *)
             
module S = Set.Make (String)

class x86env =
object(self)
  val    local_vars = ref S.empty
  method local x    = local_vars := S.add x !local_vars
  method local_vars = S.elements !local_vars
                                 
  val    allocated  = ref 0
  method allocate n = allocated := max (n + 1) !allocated
  method allocated  = !allocated
end

let allocate env stack =
  match stack with
  | []                                -> rff
  | (S n)::_                          -> env#allocate (n+1); S (n+1)
  | (R n)::_ when n < num_of_regs - 1 -> R (n+1)
  | _                                 -> env#allocate (0); S 0

module Show =
  struct

    let opnd = function
    | R i -> x86regs.(i)
    | S i -> Printf.sprintf "-%d(%%ebp)" (i * word_size)
    | M x -> x
    | L i -> Printf.sprintf "$%d" i

    let binop_to_x86 = function
      | "+"  -> "addl"
      | "-"  -> "subl"
      | "*"  -> "imull"
      | "="  -> "cmpl"
      | "->" -> "movl"

    let set_to_x86 = function
      | "<=" -> "le"
      | "<"  -> "l"
      | "==" -> "e"
      | "!=" -> "ne"
      | ">=" -> "ge"
      | ">"  -> "g"

    let instr = function
      | X86Binop (o, x, y) -> Printf.sprintf "\t%s\t%s,\t%s" (binop_to_x86 o) (opnd x) (opnd y)
      | X86Div x           -> Printf.sprintf "\tidivl\t%s" (opnd x)
      | X86Push x          -> Printf.sprintf "\tpushl\t%s" (opnd x)
      | X86Pop x           -> Printf.sprintf "\tpopl\t%s" (opnd x)
      | X86Cdq             -> "\tcdq"
      | X86Set (o, s)      -> Printf.sprintf "\tset%s\t%%%s" (set_to_x86 o) s
      | X86Call s          -> Printf.sprintf "\tcall\t%s" s
      | X86Lbl s           -> Printf.sprintf "%s:" s
      | X86Jmp s           -> Printf.sprintf "\tjmp\t%s" s
      | X86CJmp (s, l)     -> Printf.sprintf "\tj%s\t%s" s l
                                          
  end

module Compile =
  struct

    open StackMachine.Instrs

    let stack_program env code =
      let rec compile stack code =
	match code with
	| []       -> []
	| i::code' ->
	   let (stack', x86code) =
             match i with
             | S_READ ->
                let s = allocate env stack in
                (s::stack, [X86Call "read"; X86Binop ("->", eax, s)])
             | S_WRITE ->
                let s::stack' = stack in
                (stack', [X86Push s; X86Call "write"; X86Pop s])
             | S_PUSH n ->
		let s = allocate env stack in
		(s::stack, [X86Binop ("->", L n, s)])
             | S_LD x ->
                env#local x;
                let s = allocate env stack in
                (*(s::stack, match s with
                           | R _ -> [X86Binop ("->", M x, s)]
                           | _ -> [X86Binop ("->", M x, eax); X86Binop ("->", eax, s)])*)
                (s::stack, [X86Binop ("->", M x, eax); X86Binop ("->", eax, s)])
             | S_ST x ->
                env#local x;
                let s::stack' = stack in
                (stack', [X86Binop ("->", s, M x)])
             | S_BINOP o ->
                (
                  let l::r::stack' = stack
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
                         (r::stack', prereq @ cmd (l', r) o)
                )
             | S_LBL s -> (stack, [X86Lbl s])
             | S_JMP l -> (stack, [X86Jmp l])
             | S_CJMP (c, l) ->
                let s::stack' = stack in
                (stack', [X86Binop ("->", s, eax); X86Binop ("=", L 0, eax); X86CJmp (c, l)])
             | _ -> (stack, [])
           in
	   x86code @ compile stack' code'
      in
      compile [] code
              
  end

(* .comm vs .lcomm *)
(* Нужно поробовать сначала без этого *)
    
let compile prog =
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
  Buffer.contents asm
                  
let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (compile stmt);
  close_out outf;
  let runtime_o = (try Sys.getenv "RUNTIME_O" with | Not_found -> failwith "Please, provide a runtime.o file!") in
  let gcc_flags = "-Ofast" in
  ignore (Sys.command (Printf.sprintf "gcc %s -m32 -o %s %s %s.s" gcc_flags name runtime_o name))
