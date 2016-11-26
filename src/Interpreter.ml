module V = Language.Value

module M = BatMap.Make(String)
class env input = object
  inherit Stdlib.core input
  val sf : V.t M.t list ref = ref [M.empty] (* stack frames *)
  val funcs : (V.t list -> V.t) M.t ref = ref M.empty (* funcs map *)
  method new_frame = sf := M.empty::!sf
  method del_frame = let _::sf' = !sf in sf := sf'
  method get_v x = let vm::_ = !sf in M.find x vm
  method set_v x v = let vm::sf' = !sf in sf := (M.add x v vm)::sf'
  method get_f x = try Some (M.find x !funcs) with _ -> None
  method set_f x f = funcs := M.add x f !funcs
end
                    
let run input (fdefs, main) =
  let env = new env input in
  let rec expr =
    let open Language.Expr in
    function
    | Const n -> n
    | Var x -> env#get_v x
    | Binop (o, l, r) -> Op.eval_binop o (expr l) (expr r)
    | FCall (name, args) ->
       let vals = List.map (fun arg -> expr arg) args in
       match env#get_f name with
       | Some f -> f vals
       | None -> env#builtin name vals
  in
  let rec stmt =
    let open Language.Stmt in
    function
    | Skip -> None
    | Assign(x, e) -> env#set_v x @@ expr e; None
    | Seq (l, r) -> (match stmt l with | None -> stmt r | Some _ as res -> res)
    | While (e, s) as st -> if (V.to_bool @@ Op.eval_binop "!=" (expr e) V.zero) then stmt @@ Seq (s, st) else None
    | If (e, s1, s2) -> stmt @@ if (V.to_bool @@ Op.eval_binop "!=" (expr e) V.zero) then s1 else s2
    | FCall (name, args) -> expr @@ Language.Expr.FCall (name, args); None
    | Return e -> Some (expr e)
  in
  let to_exec (name, args, body) = fun vals ->
    env#new_frame;
    List.iter2 (fun x v -> env#set_v x v) args vals;
    let ret = BatOption.get @@ stmt body in
    env#del_frame;
    ret
  in
  List.iter (fun (name, _, _) as fdef -> env#set_f name @@ to_exec fdef) fdefs;
  stmt main;
  env#get_os
