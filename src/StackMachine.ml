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
  val run : int list -> (string * string list * Instrs.t list) list * Instrs.t list -> int list
end =
  struct
    module M = Map.Make (String)

    class env code labels input = object
      val cn : Instrs.t array = Array.of_list code (* Instrs.t array of stack machine *)
      val lm : int M.t = labels (* labels to line number map *)
      val mutable sf : int M.t list = [M.empty] (* stack frames list *)
      val mutable st : int list = [] (* stack *)
      val mutable is : int list = input (* input stream *)
      val mutable os : int list = [] (* output stream *)
      method get_ci ln  = if ln < Array.length cn then Some cn.(ln) else None
      method read = let i::is' = is in st <- i::st; is <- is'
      method write = let i::st' = st in st <- st'; os <- i::os
      method push n = st <- n::st
      method ld x = let vm::_ = sf in st <- (M.find x vm)::st
      method st x = let (vm::sf', y::st') = (sf, st) in sf <- (M.add x y vm)::sf'; st <- st'
      method binop o = let r::l::st' = st in st <- (Interpreter.Expr.eval_binop o l r)::st'
      method pop = let i::st' = st in st <- st'; i
      method new_frame = sf <- M.empty::sf
      method del_frame = let _::sf' = sf in sf <- sf'
      method goto l = M.find l lm
      method get_os = List.rev os
    end
       
    open Instrs
    let preprocess code =
      let rec labels ln = function
        | [] -> [], M.empty
        | i::code' ->
           let ln' =
             match i with
             | S_LBL _ -> ln
             | _ -> ln + 1
           in
           let code, labels = labels ln' code' in
           match i with
           | S_LBL l -> code, M.add l ln labels
           | _ -> i::code, labels
      in
      labels 0 code
    let run input (s_fdefs, s_main) =
      let fdefs_code = List.fold_left
                         (fun fdefs_code (name, _, s_body) -> fdefs_code @ (S_LBL name)::s_body)
                         []
                         s_fdefs
      in
      let code = [S_JMP "main"] @ fdefs_code @ [S_LBL "main"] @ s_main in
      let code, labels = preprocess code in
      let env = new env code labels input in
      let rec run' ln =
        let io = env#get_ci ln in
        match io with
        | None -> ()
        | Some i ->
           match i with
           | S_JMP l -> run' @@ env#goto l
           | S_CJMP (c, l) ->
              let x = env#pop in
              let d =
                match c with
                | "z" -> x = 0
                | "nz" -> x <> 0
              in
              run' @@ if d then env#goto l else (ln + 1)
           | S_CALL (name, args) ->
              env#new_frame;
              (List.iter
                 (fun arg -> env#st arg)
                 args);
              env#push (ln + 1);
              run' @@ env#goto name
           | S_RET ->
              env#del_frame;
              let rv = env#pop in
              let rln = env#pop in
              env#push rv;
              run' rln
           | S_END -> ()
           | _ ->
              (match i with
               | S_READ -> env#read
               | S_WRITE -> env#write
               | S_PUSH n -> env#push n
               | S_LD x -> env#ld x
               | S_ST x -> env#st x
               | S_BINOP o -> env#binop o);
              run' (ln + 1)
      in
      run' 0;
      env#get_os
  end

module Compile : sig
  val prog : Language.Prog.t -> (string * string list * Instrs.t list) list * Instrs.t list
end =
  struct
    open Instrs
    open Language.Expr
    open Language.Stmt
    module M = Map.Make (String)

    class next_val =
    object(self)
      val mutable v = -1
      method next =
        v <- v + 1;
        Printf.sprintf "lbl%d" v
    end
      
    let prog (fdefs, main) =
      let funcs = List.fold_left
                    (fun funcs (name, args, _) -> M.add name args funcs)
                    M.empty
                    fdefs
      in
      let nv = new next_val in
      let rec expr = function
        | Var x -> [S_LD x]
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
        | FCall (name, args) ->
           let s_push_args = List.concat @@ List.rev @@ List.map (fun arg -> expr arg) args in
           s_push_args @ [S_CALL (name, M.find name funcs)]
      in
      let rec stmt stmt' =
        match stmt' with
        | Skip -> []
        | Assign (x, e) -> expr e @ [S_ST x]
        | Read x -> [S_READ; S_ST x]
        | Write e -> expr e @ [S_WRITE]
        | Seq (l, r) -> stmt l @ stmt r
        | While _ | If _ ->
           (let lbl1 = nv#next in
            let lbl2 = nv#next in
            match stmt' with
            | While  (e, s) ->
               [S_LBL lbl1] @ expr e @ [S_CJMP ("z", lbl2)] @ stmt s @ [S_JMP lbl1] @ [S_LBL lbl2]
            | If (e, sy, sn) ->
               expr e @ [S_CJMP ("z", lbl2)] @ stmt sy @ [S_JMP lbl1] @ [S_LBL lbl2] @ stmt sn @ [S_LBL lbl1])
        | FCall (name, args) -> expr @@ Language.Expr.FCall (name, args)
        | Return e -> expr e @ [S_RET]
      in
      let fdef (name, args, body) = name, args, stmt body in
      let s_fdefs = List.map
                      (fun fdef' -> fdef fdef')
                      fdefs
      in
      s_fdefs, (stmt main) @ [S_END]
  end
