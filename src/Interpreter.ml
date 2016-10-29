module Env : sig
  type t
  val create_is : int list -> t
  val set_v : t -> string -> int -> t
  val get_v : t -> string -> int                      
  val clear_vm : t -> t
  val set_f : t -> string -> (t -> int list -> t * int) -> t
  val get_f : t -> string -> (t -> int list -> t * int)
  val take_i : t -> t * int
  val add_o : t -> int -> t
  val get_os : t -> int list
  val update_ios : t -> t -> t
end =
  struct
    module M = Map.Make (String)
    type t = int M.t * (t -> int list -> t * int) M.t * int list * int list
    let create_is is = (M.empty, M.empty, is, [])
    let set_v (vm, fm, is, os) kv v = (M.add kv v vm, fm, is, os)
    let get_v (vm, _, _, _) kv = M.find kv vm
    let clear_vm (_, fm, is, os) = (M.empty, fm, is, os)
    let set_f (vm, fm, is, os) kf f = (vm, M.add kf f fm, is, os)
    let get_f (_, fm, _, _) kf = M.find kf fm
    let take_i (vm, fm, i::is, os) = (vm, fm, is, os), i
    let add_o (vm, fm, is, os) o = (vm, fm, is, o::os)
    let get_os (_, _, _, os) = List.rev os
    let update_ios (vm, fm, _, _) (_, _, is, os) = (vm, fm, is, os)
  end   
module Expr : sig
  val eval_binop : string -> int -> int -> int
  val eval : Env.t -> Language.Expr.t -> Env.t * int
end =
  struct
    let eval_binop o l r =
      let bool_to_int = function true -> 1 | _ -> 0 in
      match o with
      | "+" -> l + r
      | "-" -> l - r
      | "*" -> l * r
      | "/" -> l / r
      | "%" -> l mod r
      | _ ->
         let e = match o with
           | "<=" -> l <= r
           | "<" -> l < r
           | "==" -> l = r
           | "!=" -> l <> r
           | ">=" -> l >= r
           | ">" -> l > r
           | "&&" -> (l <> 0) && (r <> 0)
           | "!!" -> (l <> 0) || (r <> 0)
         in bool_to_int e
    let rec eval env (expr : Language.Expr.t) =
      match expr with
      | Const n -> env, n
      | Var x -> env, Env.get_v env x
      | Binop (o, l, r) ->
         let env, retl = eval env l
         in
         let env, retr = eval env r
         in
         env, eval_binop o retl retr
      | FCall (name, args) ->
         let env, values = List.fold_left
                              (fun (env, values) arg -> let env, ret = eval env arg in (env, ret::values))
                              (env, [])
                              args
         in
         let env', ret = (Env.get_f env name @@ Env.clear_vm env) @@ List.rev values
         in
         Env.update_ios env env', ret
  end
module Stmt : sig
  val eval : Env.t -> Language.Stmt.t -> Env.t * int option
end =
  struct
    let rec eval env (stmt : Language.Stmt.t) =
      match stmt with
      | Skip -> env, None
      | Seq (l, r) ->
         let (env, ret) as res = eval env l
         in
         (match ret with
          | None   -> eval env r
          | Some r -> res)
      | Assign (x, e) ->
         let env, ret = Expr.eval env e
         in
         Env.set_v env x ret, None
      | Write e ->
         let env, ret = Expr.eval env e
         in
         Env.add_o env ret, None
      | Read x ->
         let env, i = Env.take_i env
         in
         Env.set_v env x i, None
      | (While (e, s)) as st ->
         let env, ret = Expr.eval env e
         in
         if (ret <> 0) then eval env (Seq (s, st)) else env, None
      | If (e, s1, s2) ->
         let env, ret = Expr.eval env e
         in
         eval env (if (ret <> 0) then s1 else s2)
      | FCall (name, args) ->
         let env, _ = Expr.eval env (Language.Expr.FCall (name, args))
         in
         env, None
      | Return e ->
         let env, ret = Expr.eval env e
         in
         env, Some ret
  end   
module Prog : sig
  val eval : int list -> Language.Prog.t -> int list
end =
  struct
    let eval input (fdefs, main) =
      let to_exec_f (name, args, body) = fun env values ->
        let env = List.fold_left
                    (fun env (a, v) -> Env.set_v env a v)
                    env
                    (List.combine args values)
        in
        let env, Some ret = Stmt.eval env body
        in
        env, ret
      in
      let env = List.fold_left
                  (fun env ((name, _, _) as fdef) -> Env.set_f env name @@ to_exec_f fdef)
                  (Env.create_is input)
                  fdefs
      in
      let env, _ = Stmt.eval env main
      in
      Env.get_os env
  end
