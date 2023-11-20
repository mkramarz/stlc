open Stlc_lib

exception Stuck

let rec eval = function
  | Var x -> Var x
  | Abs (x, b) -> Abs (x, b)
  | App (f, e) -> match eval f with
    | Abs (x, b) -> eval @@ subst (e, x) b
    | _ -> raise Stuck

let rec flatten = function
  | Base -> Base
  | Arrow (l, r) -> Arrow (flatten l, flatten r)
  | UnifVar r -> begin match !r with
      | None -> UnifVar r
      | Some tp -> flatten tp
    end

let freshvar () = UnifVar (ref None)

let rec occurs r = function
  | Base -> false
  | Arrow (t1, t2) -> occurs r t1 || occurs r t2
  | UnifVar r' -> 
      if r == r' (* rare use of referential equality! *) 
      then true
      else begin match !r' with
        | None -> false
        | Some tp -> occurs r tp
      end

let rec unify tau1 tau2 =
  match tau1, tau2 with
  (* delete (don't check more generally bc refs make it weird) *)
  | Base, Base -> ()
  | Arrow (l1, r1), Arrow (l2, r2) -> unify l1 l2; unify r1 r2 (* decompose *)
  | UnifVar r, _ -> (* eliminate *)
      if occurs r tau2 then failwith "Occurs check failure!" else
        begin match !r with
          | None -> r := Some tau2
          | Some tau1' -> unify tau1' tau2
        end
  | _, UnifVar _ -> unify tau2 tau1 (* orient *)
  | _, _ -> failwith "can't unify types :("

let constrain = unify
  
type ctx = (string * tp) list
let lookup = List.assoc_opt
               
let infer t =
  let rec go g = function
    | Var x -> begin match lookup x g with
        | None -> failwith "out of scope variable!"
        | Some tp -> tp
      end
    | Abs (x, b) -> 
        let a = freshvar () in
        let bt = go ((x,a) :: g) b in
        Arrow (a, bt)
    | App (ef, ex) ->
        let tf = go g ef in
        let tx = go g ex in
        let a  = freshvar () in
        constrain (Arrow (tx, a)) tf;
        a
  in flatten (go [] t)

let run e =
  try let _ = infer e in
    eval e
  with _ -> Var "Invalid program"

