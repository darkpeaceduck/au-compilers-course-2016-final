type i =
| S_READ
| S_WRITE
| S_PUSH  of int
| S_LD    of string
| S_ST    of string
| S_BINOP of string

module Interpreter =
  struct

    open Language.Expr

    let run input code =
      let rec run' (state, stack, input, output) code =
	match code with
	| []       -> output
	| i::code' ->
	    run'
              (match i with
              | S_READ ->
		  let y::input' = input in
		  (state, y::stack, input', output)
              | S_WRITE ->
		  let y::stack' = stack in
		  (state, stack', input, output @ [y])
              | S_PUSH n ->
		  (state, n::stack, input, output)
              | S_LD x ->
		  (state, (List.assoc x state)::stack, input, output)
              | S_ST x ->
		  let y::stack' = stack in
		  ((x, y)::state, stack', input, output)
              | S_BINOP s ->
		 let r::l::stack' = stack in
                 (state, (eval_binop s l r)::stack', input, output)
              )
              code'
      in
      run' ([], [], input, []) code
	
  end

module Compile =
  struct

    open Language.Expr
    open Language.Stmt

    let rec expr = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    (* | Binop (o, l, r) -> expr l @ expr r @ [S_BINOP o]*)
    | Binop (o, l, r) ->
       let (l', r') = (expr l, expr r) in
       match o with
       | "&&" | "!!" ->
          let nesum = [S_PUSH 0] @ l' @ [S_BINOP "!="] @ [S_PUSH 0] @ r' @ [S_BINOP "!="] @ [S_BINOP "+"] in
          (match o with
           | "&&" -> [S_PUSH 2] @ nesum @ [S_BINOP "=="]
           | _ -> [S_PUSH 0] @ nesum @ [S_BINOP "<"])
       | _ -> l' @ r' @ [S_BINOP o]
                          
    let rec stmt = function
    | Skip          -> []
    | Assign (x, e) -> expr e @ [S_ST x]
    | Read    x     -> [S_READ; S_ST x]
    | Write   e     -> expr e @ [S_WRITE]
    | Seq    (l, r) -> stmt l @ stmt r

  end
