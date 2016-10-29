module Print : sig
  val prog : Language.Prog.t -> unit
  val ints : int list -> unit
  val instrs : StackMachine.Instrs.t list -> unit
end =
  struct
    let prog _ = Printf.printf "Not finished yet. Implement it at src/PrettyPrinter.ml, if needed.\n"
    let ints l = List.iter (fun i -> Printf.printf "%d\n" i) l
    let instr (instr : StackMachine.Instrs.t) =
      match instr with
      | S_READ -> "S_READ"
      | S_WRITE -> "S_WRITE"
      | S_PUSH n -> Printf.sprintf "S_PUSH %d" n
      | S_LD s -> Printf.sprintf "S_LD %s" s
      | S_ST s -> Printf.sprintf "S_ST %s" s
      | S_BINOP s -> Printf.sprintf "S_BINOP %s" s
      | S_LBL s -> Printf.sprintf "S_LBL %s" s
      | S_JMP s -> Printf.sprintf "S_JMP %s" s
      | S_CJMP (s1, s2) -> Printf.sprintf "S_CJMP %s %s" s1 s2
    let instrs l = List.iter (fun i -> Printf.printf "%s\n" (instr i)) l
  end
