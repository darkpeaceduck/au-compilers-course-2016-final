open Ostap
open Matcher

module Expr =
  struct
    open Language.Expr
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
    open Language.Stmt
    ostap (
      parse: s:simple d:(-";" parse?)? {match d with | None | Some None -> s | Some (Some d) -> Seq (s, d)};
      expr: !(Expr.parse);
      simple:
        x:IDENT s:(":=" e:expr {Assign (x, e)} | "(" args:!(Util.list0 expr) ")" {FCall (x, args)}) {s}
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
    open Language.FDef
    ostap (
      arg: IDENT;
      stmt: !(Stmt.parse);
      parse: %"fun" name:IDENT -"(" args:!(Util.list0 arg) -")" %"begin" body:stmt %"end"
    )
  end

module Prog =
  struct
    open Language.Prog
    ostap (
      fdef: !(FDef.parse);
      stmt: !(Stmt.parse);
      parse: fdefs:(fdef)* main:stmt
    )
  end

module File =
  struct
    let parse infile =
      let s = Util.read infile in
      Util.parse
        (object
           inherit Matcher.t s
           inherit Util.Lexers.ident
                     ["skip"; (* basic *)
                      "if"; "then"; "elif"; "else"; "fi"; (* if *)
                      "while"; "do"; "od"; "repeat"; "until"; "for"; (* loops *)
                      "fun"; "begin"; "end"; "return"] (* fun *)
                     s
           inherit Util.Lexers.decimal s
           inherit Util.Lexers.skip
                     [Matcher.Skip.whitespaces " \t\n";
	              Matcher.Skip.lineComment "--";
	              Matcher.Skip.nestedComment "(*" "*)"]
                     s
         end)
        (ostap (!(Prog.parse) -EOF))
  end
