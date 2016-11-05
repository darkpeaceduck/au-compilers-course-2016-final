(* //TODO переписать !! and && в x86 для ускорения *)
(* //TODO иззбавиться от второго аругмента в call на стадии fdefs *)
(* //TODO чистить стэк после вызова функции как процедуры *)
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
module Ms = Map.Make (String)
module Mi = Map.Make (struct type t = int let compare = compare end)
module Lbl =
  struct
    let to_lbl v = Printf.sprintf "lbl%d" v
  end
module Interpreter : sig
  val preprocess : Instrs.t list -> (Instrs.t * int) list Ms.t * (Instrs.t * int) list Mi.t * (Instrs.t * int) list
  val run : int list -> Instrs.t list -> int list
end =
  struct
    module Env : sig
      type t
      val init : (Instrs.t * int) list Ms.t -> (Instrs.t * int) list Mi.t -> int list -> t
      val read : t -> t
      val write : t -> t
      val push : t -> int -> t
      val pop : t -> t * int
      val ld : t -> string -> t
      val st : t -> string -> t
      val binop : t -> string -> t
      val goto_l : t -> string -> (Instrs.t * int) list
      val goto_n : t -> int -> (Instrs.t * int) list
      val ret : t -> int * int * t
      val new_frame : t -> t
      val get_os : t -> int list
    end =
      struct
        (* stack frames * labels map * returns map * stack * input stream * output stream *)
        type t = int Ms.t list * (Instrs.t * int) list Ms.t * (Instrs.t * int) list Mi.t * int list * int list * int list
        let init labels returns is = ([Ms.empty], labels, returns, [], is, [])
        let read (sf, lm, nm, st, i::is, os) = (sf, lm, nm, i::st, is, os)
        let write (sf, lm, nm, x::st, is, os) = (sf, lm, nm, st, is, x::os)
        let push (sf, lm, nm, st, is, os) x = (sf, lm, nm, x::st, is, os)
        let pop (sf, lm, nm, x::st, is, os) = (sf, lm, nm, st, is, os), x
        let ld ((vm::_) as sf, lm, nm, st, is, os) x = (sf, lm, nm, (Ms.find x vm)::st, is, os)
        let st (vm::sf, lm, nm, y::st, is, os) x = ((Ms.add x y vm)::sf, lm, nm, st, is, os)
        let binop (sf, lm, nm, r::l::st, is, os) o = (sf, lm, nm, (Interpreter.Expr.eval_binop o l r)::st, is, os)
        let goto_l (_, lm, _, _, _, _) l = Ms.find l lm
        let goto_n (_, _, nm, _, _, _) n = Mi.find n nm
        let ret (_::sf, lm, nm, r::v::st, is, os) = r, v, (sf, lm, nm, st, is, os)
        let new_frame (sf, lm, nm, st, is, os) = (Ms.empty::sf, lm, nm, st, is, os)
        let get_os (_, _, _, _, _, os) = List.rev os
      end
    open Instrs
    let preprocess code =
      let rec labels n = function
        | [] -> Ms.empty, []
        | i::code' ->
           let n' =
             match i with
             | S_LBL _ -> n
             | _ -> n + 1
           in
           let labels, code = labels n' code'
           in
           match i with
           | S_LBL s -> Ms.add s code labels, code
           | _ -> labels, (i, n)::code
      in
      let labels, code = labels 0 code
      in
      let rec returns = function
        | [] -> Mi.empty
        | (i, n)::code' ->
           let returns = returns code'
           in
           match i with
           | S_CALL _ -> Mi.add (n + 1) code' returns
           | _ -> returns
      in
      let returns = returns code
      in
      labels, returns, code
    let run input code =
      let labels, returns, code = preprocess code   
      in
      let rec run' env = function
        | [] -> env
        | (i, linum)::code' ->
           match i with
           | S_JMP l -> run' env @@ Env.goto_l env l
           | S_CJMP (c, l) ->
              let env, x = Env.pop env
              in
              let d =
                match c with
                | "z" -> x = 0
                | "nz" -> x <> 0
              in
              run' env (if d then Env.goto_l env l else code')
           | S_CALL (name, args) ->
              let env = Env.new_frame env
              in
              let env = List.fold_left
                          (fun env arg -> Env.st env arg)
                          env
                          args
              in
              let env = Env.push env (linum + 1)
              in
              run' env @@ Env.goto_l env name
           | S_RET ->
              let ret, num, env = Env.ret env
              in
              let env = Env.push env ret
              in
              run' env @@ Env.goto_n env num
           | S_END -> env
           | _ ->
              let env =
                match i with
                | S_READ -> Env.read env
                | S_WRITE -> Env.write env
                | S_PUSH n -> Env.push env n
                | S_LD x -> Env.ld env x
                | S_ST x -> Env.st env x
                | S_BINOP o -> Env.binop env o
              in
              run' env code'
      in
      Env.get_os @@ run' (Env.init labels returns input) @@ Ms.find "main" labels
  end
module Compile : sig
  val prog : Language.Prog.t -> Instrs.t list
end =
  struct
    module M = Map.Make (String)
    open Instrs
    open Language.Expr
    open Language.Stmt
    class next_val =
    object(self)
      val mutable v = -1
      method next =
        v <- v + 1;
        v
    end
    let prog (fdefs, main) =
      let funcs = List.fold_left
                    (fun funcs (name, args, _) -> M.add name args funcs)
                    M.empty
                    fdefs
      in
      let nv = new next_val
      in
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
           let s_push_args = List.concat @@ List.rev @@ List.map (fun arg -> expr arg) args
           in
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
           (let lbl1 = Lbl.to_lbl nv#next
            in
            let lbl2 = Lbl.to_lbl nv#next
            in
            match stmt' with
            | While  (e, s) ->
               [S_LBL lbl1] @ expr e @ [S_CJMP ("z", lbl2)] @ stmt s @ [S_JMP lbl1] @ [S_LBL lbl2]
            | If (e, sy, sn) ->
               expr e @ [S_CJMP ("z", lbl2)] @ stmt sy @ [S_JMP lbl1] @ [S_LBL lbl2] @ stmt sn @ [S_LBL lbl1])
        | FCall (name, args) -> expr @@ Language.Expr.FCall (name, args)
        | Return e -> expr e @ [S_RET]
      in
      let fdef (name, _, body) = [S_LBL name] @ (stmt body)
      in
      let s_fdefs = List.fold_left
                      (fun s_fdefs fdef' -> s_fdefs @ (fdef fdef'))
                      []
                      fdefs
      in
      s_fdefs @ [S_LBL "main"] @ (stmt main) @ [S_END]
  end
