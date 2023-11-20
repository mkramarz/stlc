module VarSet = Set.Make (String)
let add = VarSet.add
let mem = VarSet.mem
let remove = VarSet.remove
let union = VarSet.union
let singleton = VarSet.singleton

type tp = Base
        | Arrow of tp * tp
        | UnifVar of tp option ref
                   
type name = string
                         
type exp = Var of name
         | App of exp * exp
         | Abs of name * exp
               
type gen_var = {
  fresh : name -> name; (* generates a fresh name based on a given one. *)
  reset : unit -> unit (* resets the internal counter for making names. *)
}                 
  
let gen_var : gen_var =
  let counter = ref 0 in
  let fresh x = incr counter; x ^ (string_of_int (!counter)) in
  let reset () = counter := 0 in
  {fresh; reset}

(* Use this function to generate fresh vars. *)
let fresh_var = gen_var.fresh
let reset_ctr = gen_var.reset
                  
let rec fv = function
  | Var x -> singleton x
  | App (f, e) -> union (fv f) (fv e)
  | Abs (x, b) -> remove x (fv b)
                    
let rec subst ((e', x) as s) = function
  | Var y -> if x = y then e' else Var y
  | App (f, e) -> App (subst s f, subst s e)
  | Abs (y, b) ->
      if x = y then Abs (y, b) else
        let (y, b) =
          if mem y (fv b) then
            rename y b
          else (y, b) in
        Abs (y, subst s b)
and rename (x : name) (e : exp) : name * exp =
  let x' = fresh_var x in
  (x', subst (Var x', x) e) 
and rename_all (names : name list) (exp : exp) : name list * exp =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp)
