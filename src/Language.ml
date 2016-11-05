module Expr =
  struct
    type t =
      | Const of int
      | Var of string
      | Binop of string * t * t
      | FCall of string * t list
  end
module Stmt =
  struct
    type t =
      | Skip
      | Assign of string * Expr.t
      | Read of string
      | Write of Expr.t
      | Seq of t * t
      | While of Expr.t * t
      | If of Expr.t * t * t
      | FCall of string * Expr.t list
      | Return of Expr.t
  end
module FDef =
  struct
    type t = string * string list * Stmt.t
  end
module Prog =
  struct
    type t = FDef.t list * Stmt.t
  end
