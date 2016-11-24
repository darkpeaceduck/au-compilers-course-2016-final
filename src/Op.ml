module V = Language.Value

let eval_binop o l r =
  let l = V.to_int l in
  let r = V.to_int r in
  V.Int
    (match o with
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
        in BatBool.to_int e)
    
let eval_cjmp o x =
  let x = V.to_int x in
  V.Int
    (BatBool.to_int @@
       match o with
       | "==0" -> x = 0
       | "!=0" -> x <> 0)
    
