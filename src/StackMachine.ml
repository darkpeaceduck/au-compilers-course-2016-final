module Instrs =
  struct
    type t =
      | S_READ
      | S_WRITE
      | S_PUSH of int
      | S_LD of string (* put arg value on top of the stack, i.e. load *)
      | S_ST of string (* take value from top of the stack to the var, i.e. store *)
      | S_BINOP of string
      | S_LBL of string
      | S_JMP of string
      | S_CJMP of string * string
      | S_CALL of string * string list
      | S_RET
      | S_END
  end

module Interpreter =
  struct
    module M = BatMap.Make(String)
    class env code labels input = object
      val cn : Instrs.t array = Array.of_list code (* Instrs.t array of stack machine *)
      val lm : int M.t = labels (* label to line number map *)
      val sf : int M.t list ref = ref [M.empty] (* stack framse list *)
      val st : int list ref = ref [] (* stack *)
      val is : int list ref = ref input (* input stream *)
      val os : int list ref = ref [] (* output stream *)
      method get_ci ln = cn.(ln)
      method read = let i::is' = !is in st := i::!st; is := is'
      method write = let i::st' = !st in st := st'; os := i::!os
      method push n = st := n::!st
      method ld x = let vm::_ = !sf in st := (M.find x vm)::!st
      method st x = let (vm::sf', y::st') = (!sf, !st) in sf := (M.add x y vm)::sf'; st := st'
      method binop o = let r::l::st' = !st in st := (Op.eval_binop o l r)::st'
      method pop = let i::st' = !st in st := st'; i
      method new_frame = sf := M.empty::!sf
      method del_frame = let _::sf' = !sf in sf := sf'
      method goto l = M.find l lm
      method get_os = List.rev !os
    end

    let run input (s_fdefs, s_main) =
      let open Instrs in
      let code =
        let fdefs_code = List.concat @@ List.map (fun (name, _, s_body) -> (S_LBL name)::s_body) s_fdefs in
        List.concat [[S_JMP "main"]; fdefs_code; [S_LBL "main"]; s_main]
      in
      let labels = BatList.fold_lefti (fun m n i -> match i with | S_LBL l -> M.add l (n + 1) m | _ -> m) M.empty code
      in
      let env = new env code labels input in
      let rec run' ln =
        match env#get_ci ln with
        | S_END -> ()
        | _ as i ->
           run' @@
             match i with
             | S_JMP l -> env#goto l
             | S_CJMP (c, l) -> if Op.eval_cjmp c @@ env#pop then env#goto l else (ln + 1)
             | S_CALL (name, args) ->
                env#new_frame;
                List.iter (fun arg -> env#st arg) args;
                env#push (ln + 1);
                env#goto name
             | S_RET ->
                env#del_frame;
                let rv = env#pop in
                let rln = env#pop in
                env#push rv;
                rln
             | _ ->
                (match i with
                 | S_READ -> env#read
                 | S_WRITE -> env#write
                 | S_PUSH n -> env#push n
                 | S_LD x -> env#ld x
                 | S_ST x -> env#st x
                 | S_BINOP o -> env#binop o
                 | S_LBL _ -> ());
                ln + 1
      in
      run' 0;
      env#get_os
  end
    
module Compile =
  struct
    module M = BatMap.Make(String)
    class env = object
      val n : int ref = ref (-1) (* int for construct new lbl *)
      val fargs : string list M.t ref = ref M.empty (* map for get func args *)
      method new_lbl = n := !n + 1; Printf.sprintf "lbl%d" !n
      method set_fargs name args = fargs := M.add name args !fargs
      method get_fargs name = M.find name !fargs
    end
                  
    let prog (fdefs, main) =
      let env = new env in
      let open Instrs in
      let rec expr =
        let open Language.Expr in
        function
        | Var x -> [S_LD x]
        | Const n -> [S_PUSH n]
        | Binop (o, l, r) ->
           let le, re = expr l, expr r in
           (match o with
            | "&&" | "!!" ->
               let bsum = List.concat [[S_PUSH 0]; le; [S_BINOP "!="]; [S_PUSH 0]; re; [S_BINOP "!="]; [S_BINOP "+"]]
               in
               (match o with
                | "&&" -> List.concat [[S_PUSH 2]; bsum; [S_BINOP "=="]]
                | _ -> List.concat [[S_PUSH 0]; bsum; [S_BINOP "<"]])
            | _ -> List.concat [le; re; [S_BINOP o]])
        | FCall (name, args) ->
           (List.concat @@ List.rev @@ List.map (fun arg -> expr arg) args) @ [S_CALL (name, env#get_fargs name)]
      in
      let rec stmt =
        let open Language.Stmt in
        function
        | Skip -> []
        | Assign (x, e) -> expr e @ [S_ST x]
        | Read x -> [S_READ; S_ST x]
        | Write e -> expr e @ [S_WRITE]
        | Seq (l, r) -> stmt l @ stmt r
        | While _ | If _ as cyc ->
           let lbl1, lbl2 = env#new_lbl, env#new_lbl in
           (match cyc with
            | While (e, s) ->
               List.concat [[S_LBL lbl1]; expr e; [S_CJMP ("==0", lbl2)]; stmt s; [S_JMP lbl1]; [S_LBL lbl2]]
            | If (e, s1, s2) ->
               List.concat [expr e; [S_CJMP ("==0", lbl2)]; stmt s1; [S_JMP lbl1]; [S_LBL lbl2]; stmt s2; [S_LBL lbl1]])
        | FCall (name, args) -> expr @@ Language.Expr.FCall (name, args)
        | Return e -> expr e @ [S_RET]
      in
      let fdef (name, args, body) = name, args, stmt body in
      List.iter (fun (name, args, _) -> env#set_fargs name args) fdefs;
      List.map (fun fd -> fdef fd) fdefs, stmt main @ [S_END]
  end
    
