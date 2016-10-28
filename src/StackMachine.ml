module Instructions =
  struct

    type i =
      | S_READ
      | S_WRITE
      | S_PUSH  of int
      | S_LD    of string
      | S_ST    of string
      | S_BINOP of string
      | S_LBL   of string
      | S_JMP   of string
      | S_CJMP  of string * string
                              
    let debug = function
      | S_READ -> "S_READ"
      | S_WRITE -> "S_WRITE"
      | S_PUSH n -> Printf.sprintf "S_PUSH %d" n
      | S_LD s -> Printf.sprintf "S_LD %s" s
      | S_ST s -> Printf.sprintf "S_ST %s" s
      | S_BINOP s -> Printf.sprintf "S_BINOP %s" s
      | S_LBL s -> Printf.sprintf "S_LBL %s" s
      | S_JMP s -> Printf.sprintf "S_JMP %s" s
      | S_CJMP (s1, s2) -> Printf.sprintf "S_CJMP %s %s" s1 s2
                                          
  end
          
module Interpreter =
  struct

    open Instructions
    open Language.Expr
           
    let run input code =
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
                    (state, (eval_binop s l r)::stack', input, output, labels)
                )
                code'
      in
      let (labels, code') = pre code in
       run' ([], [], input, [], labels) code'
	   
  end

module Compile =
  struct

    open Instructions
    open Language.Expr
    open Language.Stmt
    open Language.FDef
    open Language.Prog

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
      let to_lbl nv = Printf.sprintf "lb%d" (nv#next) in
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
        | Repeat (s, e) ->
           let lbl1 = to_lbl nv in
           [S_LBL lbl1] @ stmt' s nv @ expr e @ [S_CJMP ("z", lbl1)]
      in
      stmt' ast (new nextVal)

    let prog (fdefs, s) = stmt s

  end
