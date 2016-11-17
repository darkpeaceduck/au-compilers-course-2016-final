module Print : sig
  val prog : Language.Prog.t -> unit
  val ints : int list -> unit
  val instrs : StackMachine.Instrs.t list -> unit
end =
  struct
    let (!) s = Printf.printf "%s" s
    let rec expr =
      let open Language.Expr in
      function
      | Const n -> Printf.printf "Const %d" n
      | Var x -> Printf.printf "Var %s" x
      | Binop (o, l, r) -> !"("; expr l; !(" " ^ o ^ " "); expr r; !")"
      | FCall (name, args) -> !name; !"("; List.iter (fun arg -> expr arg; !",") args; !")"
    let rec stmt =
      let open Language.Stmt in
      function
      | Skip -> !"Skip "; !"\n"
      | Seq (l, r) -> stmt l; stmt r
      | Assign (x, e) -> !x; !" := "; expr e; !"\n"
      | Write e -> !"Write ("; expr e; !")\n"
      | While (e, s) -> !"While "; expr e; !" do"; !"\n"; stmt s; !"od"; !"\n"
    let prog (fdefs, main) = stmt main (* //OLD Printf.printf "Not finished yet. Implement it at src/PrettyPrinter.ml, if needed.\n" *)
    let ints l = List.iter (fun i -> Printf.printf "%d\n" i) l
    let instr (instr : StackMachine.Instrs.t) =
      match instr with
      | S_READ -> "S_READ"
      | S_WRITE -> "S_WRITE"
      | S_PUSH n -> Printf.sprintf "S_PUSH %d" n
      | S_LD s -> "S_LD " ^ s
      | S_ST s -> "S_ST " ^ s
      | S_BINOP s -> "S_BINOP " ^ s
      | S_LBL s -> "S_LBL " ^ s
      | S_JMP s -> "S_JMP " ^ s
      | S_CJMP (s1, s2) -> "S_CJMP " ^ s1 ^ " " ^ s2
      | S_CALL (name, args) -> "S_CALL " ^ (String.concat " " (name::args))
      | S_RET -> "S_RET"
      | S_END -> "S_END"
    let instrs l = List.iter (fun i -> Printf.printf "%s\n" (instr i)) l
  end
