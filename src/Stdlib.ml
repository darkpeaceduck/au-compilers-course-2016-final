module V = Language.Value
class coreio input = object(self)
  val is : int list ref = ref input (* input stream *)
  val os : int list ref = ref [] (* output stream *)
  method private read = let i::is' = !is in is := is'; i
  method private write x = os := x::!os
  method get_os = List.rev !os
  method builtin name args =
    match name, args with
    | "read", [] -> V.Int (self#read)
    | "write", [V.Int x] -> self#write x; V.Int (String.length @@ BatInt.to_string x)
    | "strmake", [V.Int n; V.Int c] -> V.String (Bytes.make n @@ Char.chr c)
    | "strset", [V.String s as ss; V.Int i; V.Int c] -> Bytes.set s i @@ Char.chr c; ss
    | "strget", [V.String s; V.Int i] -> V.Int (Char.code @@ Bytes.get s i)
    | "strdup", [V.String s] -> V.String (Bytes.copy s)
    | "strcat", [V.String s1; V.String s2] -> V.String (Bytes.cat s1 s2)
    | "strcmp", [V.String s1; V.String s2] -> V.Int (Bytes.compare s1 s2)
    | "strlen", [V.String s] -> V.Int (Bytes.length s)
    | "strsub", [V.String s; V.Int i; V.Int l] -> V.String (Bytes.sub s i l)
    | _ -> raise Not_found
end
