open Ostap
open Matcher
(* все открыто, потому что непонятны типы в Ostap'e *)
module Expr =
  struct
    type t =
      | Const of int
      | Var   of string
      | Binop of string * t * t
      | FCall of string * t list
  ostap (
    parse:
    !(Ostap.Util.expr
      (fun x -> x)
      (Array.map
        (
          fun (a, s) ->
          a, List.map (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
        )
        [|
        `Lefta, ["!!"];
        `Lefta, ["&&"];
        `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
        `Lefta, ["+" ; "-"];
        `Lefta, ["*" ; "/"; "%"];
        |]
      )
      primary
    );
    primary:
      n:DECIMAL {Const n}
      | f:IDENT args:(-"(" !(Util.list0 parse) -")")? { match args with | None -> Var f | Some args -> FCall (f, args) }
      | -"(" parse -")"                                              
  )
  end
module Stmt =
  struct
    type t =
      | Skip
      | Read   of string
      | Write  of Expr.t
      | Assign of string * Expr.t
      | While  of Expr.t * t
      | If     of Expr.t * t * t
      | Seq    of t * t
      | FCall  of string * Expr.t list
      | Return of Expr.t
  ostap (
    parse: s:simple d:(-";" parse?)? {match d with | None | Some None -> s | Some (Some d) -> Seq (s, d)};
    expr: !(Expr.parse);
    simple:
      x:IDENT s:(":=" e:expr {Assign (x, e)} | "(" args:!(Util.list0 expr) ")" {FCall (x, args)}) {s}
      | %"read" "(" x:IDENT ")" {Read x}
      | %"write" "(" e:expr ")" {Write e}
      | %"skip" {Skip}
      | %"while" e:expr %"do" s:parse %"od" {While (e, s)}
      | %"if" e:expr %"then" the:parse
        elif:(%"elif" expr %"then" parse)*
        ele:(%"else" parse)?                            
        %"fi" {
          If(e, the,
            List.fold_right
            (fun (e, t) elif -> If (e, t, elif))
            elif
            (match ele with | None -> Skip | Some s -> s)
          )
       }
      | %"repeat" s:parse %"until" e:expr {Seq (s, While (Binop ("==", e, Const 0), s))}
      | %"for" i:parse "," c:expr "," s:parse %"do" b:parse %"od" {Seq (i, While (c, Seq (b, s)))}
      | %"return" e:expr {Return (e)}
  )
  end
module FDef =
  struct
    type t = string * string list * Stmt.t
    ostap (
      arg: IDENT;
      stmt: !(Stmt.parse);
      parse: %"fun" name:IDENT -"(" args:!(Util.list0 arg) -")" %"begin" body:stmt %"end"
    )
  end
module Prog =
  struct
    type t = FDef.t list * Stmt.t
    ostap (
      fdef: !(FDef.parse);
      stmt: !(Stmt.parse);
      parse: fdefs:(fdef)* main:stmt
    )
  end
