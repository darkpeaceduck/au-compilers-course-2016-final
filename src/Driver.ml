let main = ()
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
        | `Parse -> () (* | `Parse -> PrettyPrinter.Print.prog prog *)
        | `Int -> Io.write @@ Interpreter.run (Io.read ()) prog
        | `DebugSM -> () (* | `DebugSM -> PrettyPrinter.Print.instrs @@ instrs () *)
        | `SM -> Io.write @@ StackMachine.Interpreter.run (Io.read ()) @@ StackMachine.Compile.prog prog
        | `X86 -> X86.Build.run prog @@ Filename.chop_suffix filename ".expr"
        | _ -> failwith "No such mode.")
    | `Fail er -> Printf.eprintf "Parse error: %s" er
  with
    Invalid_argument _ -> Printf.printf "Usage: rc.byte <name.expr>"
