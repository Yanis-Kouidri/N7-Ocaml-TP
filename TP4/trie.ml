open Assoc
open Arbre
open Chaines

(* le type trie :
    triplet arbre,
            fonction de décomposition mot -> liste de caractères,
            fonction de recomposition liste de caractères -> mot *)
type ('a,'b) trie = Trie of ('b arbre) * ('a -> 'b list) * ('b list -> 'a)

(******************************************************************************)
(*   fonction de création d'un nouveau trie                                   *)
(*   signature  : nouveau :                                                   *)
(*          ('a -> 'b list) -> ('b list -> 'a) -> ('a, 'b) trie = <fun>       *)
(*   paramètres : - une fonction de décomposition                             *)
(*                     mot -> liste de caractères                             *)
(*                -  une fonction de recomposition                            *)
(*                     liste de caractères -> mot                             *)
(*   résultat     : un nouveau trie "vide"                                    *)
(******************************************************************************)
let nouveau fd fr = Trie(Noeud(false,[]), fd, fr)


(******************************************************************************)
(*   fonction d'ajout d'un élément dans un trie                               *)
(*   signature  : ajout : 'a -> ('a, 'b) trie -> ('a, 'b) trie = <fun>        *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le trie avec le mot ajouté                                  *)
(******************************************************************************)
let ajout mot (Trie(arbre, decompose, recompose)) =
  Trie (ajout_arbre (decompose mot) arbre,decompose,recompose)

(******************************************************************************)
(*   fonction d'appartenance d'un élément à un trie                           *)
(*   signature  : appartient : 'a -> ('a, 'b) trie -> bool = <fun>            *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le résultat booléen du test                                 *)
(******************************************************************************)
let appartient mot (Trie(arbre, decompose, _)) = 
  appartient_arbre (decompose mot) arbre

(*  Pour les tests *)
let trie_sujet =
  List.fold_right ajout
    ["bas"; "bât"; "de"; "la"; "lai"; "laid"; "lait"; "lard"; "le"; "les"; "long"]
    (nouveau decompose_chaine recompose_chaine)

let%test _ = appartient "bas" trie_sujet = true
let%test _ = appartient "base" trie_sujet = false
let%test _ = appartient "bat" trie_sujet = false
let%test _ = appartient "bât" trie_sujet = true
let%test _ = appartient "long" trie_sujet = true
let%test _ = appartient "longe" trie_sujet = false
let%test _ = appartient "lon" trie_sujet = false
let%test _ = appartient "" trie_sujet = false
let%test _ = appartient "a" trie_sujet = false


(******************************************************************************)
(*   fonction de retrait d'un élément d'un trie                               *)
(*   signature  : trie_retrait : 'a -> ('a, 'b) trie -> ('a, 'b) trie = <fun> *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le trie avec le mot retiré                                  *)
(******************************************************************************)
let retrait mot (Trie(arbre, decompose, recompose)) = 
  Trie (retrait_arbre (decompose mot) arbre, decompose, recompose)

let%test _ = appartient "bas" trie_sujet = true
let%test _ = appartient "bas" (retrait "bas" trie_sujet) = false
let%test _ = appartient "bat" (retrait "bas" trie_sujet) = false
let%test _ = appartient "base" (retrait "bas" trie_sujet) = false
let%test _ = appartient "bât" (retrait "bas" trie_sujet) = true
let%test _ = appartient "de" (retrait "bas" trie_sujet) = true
let%test _ = appartient "la" (retrait "bas" trie_sujet) = true
let%test _ = appartient "lai" (retrait "bas" trie_sujet) = true
let%test _ = appartient "lai" (retrait "lai" trie_sujet) = false
let%test _ = appartient "bas" (retrait "lai" trie_sujet) = true


(******************************************************************************)
(*   fonction interne au Module qui génère la liste de tous les mots          *)
(*   d'un trie                                                                *)
(*   signature    : trie_dico : ('a, 'b) trie -> 'a list = <fun>              *)
(*   paramètre(s) : le trie                                                   *)
(*   résultat     : la liste des mots                                         *)
(******************************************************************************)
let trie_dico trie = failwith "trie_dico"

(******************************************************************************)
(* procédure d'affichage d'un trie                                            *)
(*   signature  : affiche : ('a -> unit) -> ('a, 'b) trie -> unit = <fun>     *)
(*   paramètres : - une procédure d'affichage d'un mot                        *)
(*                - un trie                                                   *)
(*   résultat   : aucun                                                       *)
(******************************************************************************)
let affiche p (Trie(arbre, _, recompose)) = 
  let rec affiche_liste l = (*fonction auxiliaire qui traite avec la liste fournie par parcours_arbre*)
    match l with
    | [] -> () (* Cas de base () = unit*)
    | h::q ->  p (recompose h); (* Affichage du mot *)
               print_string " "; (* Affichage d'un espace *)
               affiche_liste q (*Appel recursif sur la liste fournie par parocurs_arbre*)
  in affiche_liste (parcours_arbre arbre)

  let _ = affiche print_string trie_sujet (* Test d'affichage dans la console *)