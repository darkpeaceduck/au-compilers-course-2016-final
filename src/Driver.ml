let main =
  ()
    try
      let mode, filename =
        match Sys.argv.(1) with
        | "-p" -> `Parse, Sys.argv.(2) (* -p : print ast *)
        | "-i" -> `Int, Sys.argv.(2) (* -i : interpret *)
        | "-s" ->
           (match Sys.argv.(2) with
            | "-d" -> `DebugSM , Sys.argv.(3) (* -s -d : stack machine debug *)
            | _ -> `SM , Sys.argv.(2)) (* -s : stack machine compile and interpret *)
        | "-o" -> `X86, Sys.argv.(2) (* -o : x86 compile and interpret *)
        | _ -> failwith "No such flag. It is necessary to specify the type of compiling."
      in
      match Parser.File.parse filename with
      | `Ok prog -> 
	 (match mode with
	  | `X86 ->
             let basename = Filename.chop_suffix filename ".expr" in 
	     X86.build prog basename
          | `Parse -> PrettyPrinter.Print.prog prog
	  | _ ->
	     let rec read acc =
	       try
		 let r = read_int () in
		 Printf.printf "> ";
		 read (acc @ [r]) 
               with End_of_file -> acc
	     in
	     let ints = fun () -> read [] in
             let instrs = fun () -> StackMachine.Compile.prog prog in
             match mode with
             (*| `DebugSM -> PrettyPrinter.Print.instrs @@ instrs ()*)
             | _ ->
                let output = match mode with
                  | `SM -> StackMachine.Interpreter.run (ints ()) @@ instrs ()
                  | _ -> Interpreter.Prog.eval (ints ()) prog
                in
                PrettyPrinter.Print.ints output)
      | `Fail er -> Printf.eprintf "Parse error: %s" er
    with 
    | Invalid_argument _ -> Printf.printf "Usage: rc.byte <name.expr>"
