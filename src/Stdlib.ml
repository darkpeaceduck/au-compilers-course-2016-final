module V = Language.Value
module M = BatMap.Make(String)
class core input = object(self)
  (* IO *)
  val is : int list ref = ref input (* input stream *)
  val os : int list ref = ref [] (* output stream *)
  method private read = let i::is' = !is in is := is'; i
  method private write x = os := x::!os
  method get_os = List.rev !os
  (* BUILTIN *)
  val mutable bi : (V.t list -> V.t) M.t = M.empty
  method builtin name = M.find name bi
  method get_bulitins_list = BatEnum.fold (fun l k -> k::l) [] @@ M.keys bi
  initializer
    bi <-
      List.fold_left
        (fun m (n, f) -> M.add n f m) M.empty
        [("read", function | [] -> V.Int (self#read));
         ("write", function | [V.Int x] -> self#write x; V.Int (String.length @@ BatInt.to_string x));
         ("strmake", function | [V.Int n; V.Int c] -> V.String (Bytes.make n @@ Char.chr c));
         ("strset", function | [V.String s as ss; V.Int i; V.Int c] -> Bytes.set s i @@ Char.chr c; ss);
         ("strget", function | [V.String s; V.Int i] -> V.Int (Char.code @@ Bytes.get s i));
         ("strdup", function | [V.String s] -> V.String (Bytes.copy s));
         ("strcat", function | [V.String s1; V.String s2] -> V.String (Bytes.cat s1 s2));
         ("strcmp", function | [V.String s1; V.String s2] -> V.Int (Bytes.compare s1 s2));
         ("strlen", function | [V.String s] -> V.Int (Bytes.length s));
         ("strsub", function | [V.String s; V.Int i; V.Int l] -> V.String (Bytes.sub s i l));
         ("arrlen", function | [V.Array a] -> V.Int (Array.length a));
         ("arrmake", function | [V.Int n; V.Int v as value] -> V.Array (Array.make n value));
         ("Arrmake", function | [V.Int n; value] -> V.Array (Array.make n value))]
end
                     
let builtins_list = (new core [])#get_bulitins_list
