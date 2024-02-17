(*  Exercice à rendre **)

(* absolute : int -> int
  Retourne la valeur absolu d'un entier
  a int : entier dont on veut la valeur absolue
  result : a si -a < 0  a sinon
   *)
   let absolute a =
    if a < 0
      then -a
    else a

(*  PGCD : int -> int -> int
   Retourne le PGCD de a et b 
   a int premier argument du PGCD
   b int second argument du PGCD
   result : int le pgcd de a et b*)
let rec pgcd a b = 
    let aux a b =
    if (a = 0 || b = 0)
      then max a b
    else if (a > b) 
      then pgcd (a-b) b
      else pgcd a (b-a)
    in aux (absolute a) (absolute b)   


(*tests unitaires *)
let%test _ = pgcd 0 0 = 0 (*NB : Normalement le PGCD(0,0) est indéfini*)
let%test _ = pgcd 1 1 = 1
let%test _ = pgcd 13 13 = 13
let%test _ = pgcd 0 17 = 17
let%test _ = pgcd 17 0 = 17
let%test _ = pgcd 18 4 = 2
let%test _ = pgcd 4 18 = 2
let%test _ = pgcd (-5) (-15) = 5
let%test _ = pgcd (-1) (-1) = 1
let%test _ = pgcd (-0) (-17) = 17
let%test _ = pgcd 0 (-17) = 17
let%test _ = pgcd (-0) 17 = 17
let%test _ = pgcd 5 (-15) = 5
let%test _ = pgcd (-5) 15 = 5
let%test _ = pgcd 9 21 = 3
let%test _ = pgcd (-9) 21 = 3
let%test _ = pgcd 9 (-21) = 3
let%test _ = pgcd (-9) (-21) = 3
let%test _ = pgcd (-37) (-37) = 37
