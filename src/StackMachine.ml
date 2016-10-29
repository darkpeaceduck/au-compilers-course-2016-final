module Instrs =
  struct
    type t =
      | S_READ
      | S_WRITE
      | S_PUSH of int
      | S_LD of string (* put arg value on top of the stack *)
      | S_ST of string (* take value from top of the stack to the var *)
      | S_BINOP of string
      | S_LBL of string
      | S_JMP of string
      | S_CJMP of string * string
      | S_CALL of string * string list
      | S_RET
      | S_END
  end
module Interpreter : sig
  val run : int list -> Instrs.t list -> int list
end =
  struct
    let run input code = [0; 0; 0;]
           
    (*let run input code =
      let rec pre code =
        match code with
        | [] -> ([], [])
        | i::code' ->
           let (labels, code'') = pre code' in
           match i with
           | S_LBL s -> ((s, code'')::labels, code'')
           | _ -> (labels, i::code'')                 
      in
      let rec run' ((state, stack, input, output, labels) as c) code =
	match code with
	| []       -> output
	| i::code' ->
           match i with
           | S_JMP l -> run' c (List.assoc l labels)
           | S_CJMP (s, l) ->
              let y::stack' = stack in
              run' (state, stack', input, output, labels)
                   (match s with
                    | "z" -> if y = 0 then List.assoc l labels else code'
                    | "nz" -> if y <> 0 then List.assoc l labels else code')
           | _ ->
	      run'
                (match i with
                 | S_READ ->
		    let y::input' = input in
		    (state, y::stack, input', output, labels)
                 | S_WRITE ->
		    let y::stack' = stack in
		    (state, stack', input, output @ [y], labels)
                 | S_PUSH n ->
		    (state, n::stack, input, output, labels)
                 | S_LD x ->
		    (state, (List.assoc x state)::stack, input, output, labels)
                 | S_ST x ->
		    let y::stack' = stack in
		    ((x, y)::state, stack', input, output, labels)
                 | S_BINOP s ->
		    let r::l::stack' = stack in
                    (state, (Interpreter.Expr.eval_binop s l r)::stack', input, output, labels)
                )
                code'
      in
      let (labels, code') = pre code in
       run' ([], [], input, [], labels) code'
     *)

  end
module Compile : sig
  val prog : Language.Prog.t -> Instrs.t list
end =
  struct
    
    (*open Instrs
    open Language.Expr
    open Language.Stmt
    open Language.FDef

    let rec expr = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    | Binop (o, l, r) ->
       (let (l', r') = (expr l, expr r) in
       match o with
       | "&&" | "!!" ->
          let nesum = [S_PUSH 0] @ l' @ [S_BINOP "!="] @ [S_PUSH 0] @ r' @ [S_BINOP "!="] @ [S_BINOP "+"] in
          (match o with
           | "&&" -> [S_PUSH 2] @ nesum @ [S_BINOP "=="]
           | _ -> [S_PUSH 0] @ nesum @ [S_BINOP "<"])
       | _ -> l' @ r' @ [S_BINOP o])

    class nextVal =
    object(self)
      val mutable v = 0
      method next =
        v <- v + 1;
        v
    end
         
    let stmt ast =
      let to_lbl nv = Printf.sprintf "cycle_lbl%d" (nv#next) in
      let rec stmt' ast nv =
        match ast with
        | Skip          -> []
        | Assign (x, e) -> expr e @ [S_ST x]
        | Read    x     -> [S_READ; S_ST x]
        | Write   e     -> expr e @ [S_WRITE]
        | Seq    (l, r) -> stmt' l nv @ stmt' r nv
        | While _ | If _ ->
           (let lbl1 = to_lbl nv in
            let lbl2 = to_lbl nv in
            match ast with
            | While  (e, s) ->
               [S_LBL lbl1] @ expr e @ [S_CJMP ("z", lbl2)] @ stmt' s nv @ [S_JMP lbl1] @ [S_LBL lbl2]
            | If (e, s1, s2) ->
               expr e @ [S_CJMP ("z", lbl2)] @ stmt' s1 nv @ [S_JMP lbl1] @ [S_LBL lbl2] @ stmt' s2 nv @ [S_LBL lbl1])
      in
      stmt' ast (new nextVal)*)

    open Instrs (* //TODO ? , function otherwise*)
    let rec expr expr =
      match expr with
      | Var x -> [S_LD x]
      | Const n -> [S_PUSH n]
      | Binop (o, l, r) ->
         (let (l', r') = (expr l, expr r) in
          match o with
          | "&&" | "!!" -> (* //TODO переписать в x86 для ускорения *)
             let nesum = [S_PUSH 0] @ l' @ [S_BINOP "!="] @ [S_PUSH 0] @ r' @ [S_BINOP "!="] @ [S_BINOP "+"] in
             (match o with
              | "&&" -> [S_PUSH 2] @ nesum @ [S_BINOP "=="]
              | _ -> [S_PUSH 0] @ nesum @ [S_BINOP "<"])
          | _ -> l' @ r' @ [S_BINOP o])
      | FCall (name, args) ->
         let s_push_args = List.map (fun arg -> expr arg) args
         in
         s_push_args @ [S_CALL]
    let stmt stmt = [S_RET]
    let fdef (name, args, body) =
      let s_pop_args = List.map (fun arg -> S_ST arg) args
      in
      [S_LBL name] @ s_pop_args @ (stmt body)
    let prog (fdefs, main) =
      let s_fdefs = List.fold_left
                      (fun s_fdefs fdef' -> s_fdefs @ (fdef fdef'))
                      []
                      fdefs
      in
      s_fdefs @ (stmt main) @ [S_END]
  end
