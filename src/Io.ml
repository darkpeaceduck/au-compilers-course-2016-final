let read () =
  let rec read' acc =
    try
      let r = read_int () in
      Printf.printf "> ";
      read' (acc @ [r]) 
    with End_of_file -> acc
  in
  read' []
        
let write os = List.iter (fun i -> Printf.printf "%d\n" i) os
