let eval_binop o l r =
  match o with
  | "+" -> l + r
  | "-" -> l - r
  | "*" -> l * r
  | "/" -> l / r
  | "%" -> l mod r
  | _ ->
     let e = match o with
       | "<=" -> l <= r
       | "<" -> l < r
       | "==" -> l = r
       | "!=" -> l <> r
       | ">=" -> l >= r
       | ">" -> l > r
       | _ ->
          let l = l <> 0 in
          let r = r <> 0 in
          match o with
          | "&&" -> l && r
          | "!!" -> l || r
     in BatBool.to_int e

let eval_cjmp o x =
  match o with
  | "==0" -> x = 0
  | "!=0" -> x <> 0
