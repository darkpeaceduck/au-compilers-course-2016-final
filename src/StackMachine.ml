module V = Language.Value

module Instrs =
  struct
    type t =
      | S_PUSH of V.t  
      | S_POP
      | S_LD of string (* put arg value on top of the stack, i.e. load *)
      | S_ST of string (* take value from top of the stack to the var, i.e. store *)
      | S_BINOP of string
      | S_LBL of string
      | S_JMP of string
      | S_CJMP of string * string
      | S_CALL of string * string list 
      | S_BUILTIN of string * int
      | S_RET
      | S_END
  end

module Interpreter =
  struct
    module M = BatMap.Make(String)
    class env code labels input = object
      inherit Stdlib.core input
      val cn : Instrs.t array = Array.of_list code (* Instrs.t array of stack machine *)
      val lm : int M.t = labels (* label to line number map *)
      val sf : V.t M.t list ref = ref [M.empty] (* stack framse list *)
      val st : V.t list ref = ref [] (* stack *)
      method get_ci ln = cn.(ln)
      method push n = st := n::!st
      method ld x = let vm::_ = !sf in st := (M.find x vm)::!st
      method st x = let (vm::sf', y::st') = (!sf, !st) in sf := (M.add x y vm)::sf'; st := st'
      method binop o = let r::l::st' = !st in st := (Op.eval_binop o l r)::st'
      method pop = let i::st' = !st in st := st'; i
      method new_frame = sf := M.empty::!sf
      method del_frame = let _::sf' = !sf in sf := sf'
      method goto l = M.find l lm
    end

    let run input (s_fdefs, s_main) =
      let open Instrs in
      let env =
        let code =
          let fdefs_code = List.concat @@ List.map (fun (name, _, s_body) -> (S_LBL name)::s_body) s_fdefs in
          List.concat [[S_JMP "main"]; fdefs_code; [S_LBL "main"]; s_main]
        in
        let labels = BatList.fold_lefti (fun m n i -> match i with | S_LBL l -> M.add l (n + 1) m | _ -> m) M.empty code
        in
        new env code labels input
      in
      let rec run' ln =
        match env#get_ci ln with
        | S_END -> ()
        | _ as i ->
           run' @@
             match i with
             | S_PUSH n -> env#push n; ln + 1
             | S_POP -> env#pop; ln + 1
             | S_LD x -> env#ld x; ln + 1
             | S_ST x -> env#st x; ln + 1
             | S_BINOP o -> env#binop o; ln + 1
             | S_LBL _ -> ln + 1
             | S_JMP l -> env#goto l
             | S_CJMP (c, l) -> if V.to_bool @@ Op.eval_cjmp c @@ env#pop then env#goto l else (ln + 1)
             | S_CALL (name, args) ->
                env#new_frame;
                List.iter (fun arg -> env#st arg) @@ List.rev args;
                env#push @@ V.Int (ln + 1);
                env#goto name
             | S_BUILTIN (name, argsn) ->
                let args = BatList.init argsn (fun _ -> env#pop) in
                env#push @@ env#builtin name @@ List.rev args;
                ln + 1
             | S_RET ->
                env#del_frame;
                let rv = env#pop in
                let rln = env#pop in
                env#push rv;
                V.to_int rln
      in
      run' 0;
      env#get_os
  end
    
module Compile =
  struct
    module M = BatMap.Make(String)
    class env fargs = object
      val fargs : string list M.t = fargs (* map for get func args *)
      val n : int ref = ref (-1) (* int for construct new lbl *)
      method new_lbl = n := !n + 1; Printf.sprintf "lbl%d" !n
      method get_fargs name = try Some (M.find name fargs) with _ -> None
    end

    open Language.Expr
    open Language.Stmt
    let prog (fdefs, main) =
      let env =
        let fargs = List.fold_left (fun m (name, args, _) -> M.add name args m) M.empty fdefs in
        new env fargs
      in
      let open Instrs in
      let rec expr =
        (*let open Language.Expr in*)
        function
        | Var x -> [S_LD x]
        | Const n -> [S_PUSH n]
        | Binop (o, l, r) ->
           let le, re = expr l, expr r in
           (match o with
            | "&&" | "!!" ->
               let bsum = List.concat [[S_PUSH V.zero]; le; [S_BINOP "!="]; [S_PUSH V.zero]; re; [S_BINOP "!="]; [S_BINOP "+"]]
               in
               (match o with
                | "&&" -> List.concat [[S_PUSH (V.Int 2)]; bsum; [S_BINOP "=="]]
                | _ -> List.concat [[S_PUSH V.zero]; bsum; [S_BINOP "<"]])
            | _ -> List.concat [le; re; [S_BINOP o]])
        | FCall (name, args) ->
           (List.concat @@ List.map (fun arg -> expr arg) args) @
             match env#get_fargs name with
             | Some fargs -> [S_CALL (name, fargs)]
             | None -> [S_BUILTIN (name, List.length args)]
      in
      let rec stmt =
        (*let open Language.Stmt in*)
        function
        | Skip -> []
        | Assign (x, e) -> expr e @ [S_ST x]
        | Seq (l, r) -> stmt l @ stmt r
        | While _ | If _ as cyc ->
           let lbl1, lbl2 = env#new_lbl, env#new_lbl in
           (match cyc with
            | While (e, s) ->
               List.concat [[S_LBL lbl1]; expr e; [S_CJMP ("==0", lbl2)]; stmt s; [S_JMP lbl1]; [S_LBL lbl2]]
            | If (e, s1, s2) ->
               List.concat [expr e; [S_CJMP ("==0", lbl2)]; stmt s1; [S_JMP lbl1]; [S_LBL lbl2]; stmt s2; [S_LBL lbl1]])
        | FCall (name, args) -> (expr @@ Language.Expr.FCall (name, args)) @ [S_POP]
        | Return e -> expr e @ [S_RET]
      in
      let fdef (name, args, body) = name, args, stmt body in
      List.map (fun fd -> fdef fd) fdefs, stmt main @ [S_END]
  end
