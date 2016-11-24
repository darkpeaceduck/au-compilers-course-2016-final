module Value =
  struct
    type t =
      | Int of int
      | String of bytes
                    
    let zero, one = Int 0, Int 1
                               
    let to_int = function
      | Int i -> i
      | _ -> failwith "Not an Int!"

    let to_bool = function
      | Int i -> i <> 0
      | _ -> failwith "Not an Int!"
  end

module Expr =
  struct
    type t =
      | Const of Value.t
      | Var of string
      | Binop of string * t * t
      | FCall of string * t list
  end

module Stmt =
  struct
    type t =
      | Skip
      | Assign of string * Expr.t
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
    
