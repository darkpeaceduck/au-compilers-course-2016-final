type opnd = R of int | S of int | M of string | L of int

let x86regs = [|
    "%eax";
    "%edx";
    "%ebx";
    "%ecx";
    "%esi";
    "%edi"
  |]

let num_of_regs = Array.length x86regs
let ff = 2
let rff = R ff
let word_size = 4

let eax = R 0
let edx = R 1
let ebx = R 2
let ecx = R 3
let esi = R 4
let edi = R 5

type prei =
  | Add
  | Sub
  | Mul
  | Cmp
  | Mov
  | Div
  | Push
  | Pop

let to_str = function
  | Add  -> "addl"
  | Sub  -> "subl"
  | Mul  -> "imull"
  | Cmp  -> "cmpl"
  | Mov  -> "movl"
  | Div  -> "idivl"
  | Push -> "pushl"
  | Pop  -> "popl"
            
type instr =
  | X86Dop  of prei * opnd * opnd (* add, sub, mul, cmp, mov *)
  | X86Sop  of prei * opnd (* div, push, pop *)
  | X86Cdq
  | X86Set  of string * string (* <= < == != >= > *)
  | X86Call of string
  | X86Lbl  of string
  | X86Jmp  of string
  | X86J    of string * string
             
module S = Set.Make (String)

class x86env =
object(self)
  val    local_vars = ref S.empty
  method local x    = local_vars := S.add x !local_vars
  method local_vars = S.elements !local_vars
                                 
  val    allocated  = ref 0
  method allocate n = allocated := max n !allocated
  method allocated  = 1 + !allocated
end

let allocate env stack =
  match stack with
  | []                              -> R ff
  | (S n)::_                        -> env#allocate (n+1); S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> S 0

module Show =
  struct

    let opnd = function
    | R i -> x86regs.(i)
    | S i -> Printf.sprintf "-%d(%%ebp)" (i * word_size)
    | M x -> x
    | L i -> Printf.sprintf "$%d" i

    let instr = function
      | X86Dop (s, x, y) -> Printf.sprintf "\t%s\t%s,\t%s" (to_str s) (opnd x) (opnd y)
      | X86Sop (s, x)    -> Printf.sprintf "\t%s\t%s" (to_str s) (opnd x)
      | X86Cdq           -> "\tcdq"
      | X86Set (s1, s2)  -> Printf.sprintf "\tset%s\t%%%s" (s1) (s2)
      | X86Call s        -> Printf.sprintf "\tcall\t%s" s
      | X86Lbl s         -> Printf.sprintf "%s:" s
      | X86Jmp s         -> Printf.sprintf "\tjmp\t%s" s
      | X86J   (s, l)    -> Printf.sprintf "\tj%s\t%s" s l
                                          
  end

module Compile =
  struct

    open StackMachine.Instructions

    let stack_program env code =
      let rec compile stack code =
	match code with
	| []       -> []
	| i::code' ->
	   let (stack', x86code) =
             match i with
             | S_READ ->
                let s = allocate env stack in
                (s::stack, [X86Call "read"; X86Dop (Mov, eax, s)])
             | S_WRITE ->
                let s::stack'' = stack in
                (stack'', [X86Sop (Push, s); X86Call "write"; X86Sop (Pop, s)]) 
             | S_PUSH n ->
		let s = allocate env stack in
		(s::stack, [X86Dop (Mov, L n, s)])
             | S_LD x ->
                env#local x;
                let s = allocate env stack in
                (s::stack, match s with
                           | R _ -> [X86Dop (Mov, M x, s)]
                           | _ -> [X86Dop (Mov, M x, eax); X86Dop (Mov, eax, s)])
             | S_ST x   ->
                env#local x;
                let s::stack' = stack in
                (stack', [X86Dop (Mov, s, M x)])
             | S_BINOP o ->
                (let l::r::stack' = stack in
                let rec ifnreg (x,y) = match x,y with
                  | R _, _ | _, R _ -> ([], x)
                  | _ -> ([X86Dop (Mov, x, edx)], edx)
                in
                let cmpop = function
                  | "<=" -> "le" | "<"  -> "l"  | "==" -> "e"
                  | "!=" -> "ne" | ">=" -> "ge" | ">"  -> "g"
                in
                let cmd (x,y) = function
                  | "+" -> [X86Dop (Add, x, y)]
                  | "-" -> [X86Dop (Sub, x, y)]
                  | "*" -> (match x,y with | _, R _ -> [X86Dop (Mul, x, y)] | _ -> [X86Dop (Mul, y, x); X86Dop (Mov, x, y)])
                  | op -> [X86Dop (Mov, L 0, eax); X86Dop (Cmp, x, y); X86Set (cmpop(op), "al"); X86Dop (Mov, eax, y)]
                in
                match o with
                | "/" | "%" -> (r::stack', [X86Dop (Mov, r, eax); X86Cdq; X86Sop (Div, l); X86Dop (Mov, (if (o = "/") then eax else edx), r)])
                | _ -> let (p, l') = ifnreg (l, r)
                       in (r::stack', p @ cmd (l', r) (o)))
             | S_LBL s -> (stack, [X86Lbl s])
             | S_JMP l -> (stack, [X86Jmp l])
             | S_CJMP (s, l) ->
                let y::stack' = stack in
                (stack', [X86Dop (Mov, y, eax); X86Dop (Cmp, L 0, eax); X86J (s, l)])
           in
	   x86code @ compile stack' code'
      in
      compile [] code
              
  end
    
let compile stmt =
  let env = new x86env in
  let code = Compile.stack_program env @@ StackMachine.Compile.stmt stmt in
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
  ignore (Sys.command (Printf.sprintf "gcc -m32 -o %s %s %s.s" name runtime_o name))
