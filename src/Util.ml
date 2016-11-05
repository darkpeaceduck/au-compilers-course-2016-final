module List =
  (**
   * @pararm a list of elements
   * @return the list of pairs of (element, index) starting from (_, 0)
   *)
  struct
    let indexing list =
      let rec indexing' n = function
        | [] -> []
        | x::xs -> (x, n)::(indexing' (n + 1) xs)
      in
      indexing' 0 list
  end
