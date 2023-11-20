open Angstrom
open Stlc_lib

(* Parsing *)

exception ParseError

let parens p = char '(' *> p <* char ')'
let ws = skip_while (function ' ' | '\t' | '\n' -> true | _ -> false)
let name = take_while1 (function 'a'..'z' | 'A'..'Z' -> true | _ -> false)
let var = name >>| fun x -> Var x
let lam expr = char '\\' *> name >>= (fun x -> char '.' *> expr >>| (fun b -> Abs (x, b)))


let exp' exp = ws *> (parens exp <|> var <|> lam exp)
let exp = fix @@ fun exp -> ws *> (exp' exp >>= (fun e -> exp >>| (fun e' -> App (e, e'))) <|> exp' exp) <* ws


(* Printing *)

let wrap f = function
| Var x -> x
| e -> "(" ^ (f e) ^ ")"

let rec string_of_exp = function
| Var x -> x
| App (f, e) -> (wrap string_of_exp f) ^ " " ^ (wrap string_of_exp e)
| Abs (x, b) -> "\\" ^ x ^ ". " ^ (wrap string_of_exp b)
