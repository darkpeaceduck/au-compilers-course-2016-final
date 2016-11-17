module M = BatMap.Make(String)
class env input = object
  val funcs : (int list -> int) M.t ref = ref M.empty (* funcs map *)
  val sf : int M.t list ref = ref [M.empty] (* stack frames *)
  val is : int list ref = ref input (* input stream *)
  val os : int list ref = ref [] (* output stream *)
  method new_frame = sf := M.empty::!sf
  method del_frame = let _::sf' = !sf in sf := sf'
  method get_v x = let vm::_ = !sf in M.find x vm
  method set_v x v = let vm::sf' = !sf in sf := (M.add x v vm)::sf'
  method get_f x = M.find x !funcs
  method set_f x f = funcs := M.add x f !funcs
  method take_i = let i::is' = !is in is := is'; i
  method add_o x = os := x::!os
  method get_os = List.rev !os
end

let run input (fdefs, main) =
  let env = new env input in
  let rec expr =
    let open Language.Expr in
    function
    | Const n -> n
    | Var x -> env#get_v x
    | Binop (o, l, r) -> Op.eval_binop o (expr l) (expr r)
    | FCall (name, args) -> let vals = List.map (fun arg -> expr arg) args in env#get_f name @@ List.rev vals
  in
  let rec stmt =
    let open Language.Stmt in
    function
    | Skip -> None
    | Assign(x, e) -> env#set_v x @@ expr e; None
    | Read x -> env#set_v x @@ env#take_i; None
    | Write e -> env#add_o @@ expr e; None
    | Seq (l, r) -> (match stmt l with | None -> stmt r | Some _ as res -> res)
    | While (e, s) as st -> if (expr e <> 0) then stmt @@ Seq (s, st) else None
    | If (e, s1, s2) -> stmt @@ if (expr e <> 0) then s1 else s2
    | FCall (name, args) -> expr @@ Language.Expr.FCall (name, args); None
    | Return e -> BatOption.some @@ expr e
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
