open Assoc

type 'a arbre = Noeud of bool * ( ('a branche) list)
and 'a branche = 'a * 'a arbre

(* Pour les tests *)
let bb = ('b',Noeud(false,[('a',Noeud(false,[('s',Noeud(true,[]));('t',Noeud(true,[]))]))]))
let bd = ('d',Noeud(false,[('e',Noeud(true,[]))]))
let bl = ('l',Noeud(false,[('a',Noeud(true,[('i',Noeud(true,[('d',Noeud(true,[]));('t',Noeud(true,[]))]));('r',Noeud(false,[('d',Noeud(true,[]))]))]));
                           ('e',Noeud(true,[('s',Noeud(true,[]))]));
                           ('o',Noeud(false,[('n',Noeud(false,[('g',Noeud(true,[]))]))]))]))
let b1 = [bb;bd;bl]
let arbre_sujet = Noeud(false,b1)

(******************************************************************************)
(*   fonction d'appartenance d'une liste d'éléments à un arbre                *)
(*   signature  : appartient : 'a list -> 'a arbre -> bool                    *)
(*   paramètres : - une liste d'éléments (caractères dans le cas d'un dico)   *)
(*                - un arbre n-aire                                           *)
(*   résultat   : le résultat booléen du test                                 *)
(******************************************************************************)
let rec appartient_arbre lc (Noeud (b,lb)) =
  match lc with
  (* on a épuisé la liste : le résultat est le booléen du noeud sur
     lequel on est arrivé *)
  | [] -> b
  (* sinon on cherche la branche correspondant au premier
     caractère de la liste :
     - elle n'existe pas : le mot n'appartient pas au trie
     - on la trouve, on relance aux avec le reste de la liste
     et l'arbre de cette branche *)
  | c::qlc ->
     match recherche c lb with
     | None -> false
     | Some a -> appartient_arbre qlc a

let%test _ = appartient_arbre ['b';'a';'s']  arbre_sujet
let%test _ = appartient_arbre ['b';'a';'t']  arbre_sujet
let%test _ = appartient_arbre ['d';'e']  arbre_sujet
let%test _ = appartient_arbre ['l';'a']  arbre_sujet
let%test _ = appartient_arbre ['l';'a';'i']  arbre_sujet
let%test _ = appartient_arbre ['l';'a';'i';'d']  arbre_sujet
let%test _ = appartient_arbre ['l';'a';'i';'t']  arbre_sujet
let%test _ = appartient_arbre ['l';'a';'r';'d']  arbre_sujet
let%test _ = appartient_arbre ['l';'e']  arbre_sujet
let%test _ = appartient_arbre ['l';'e';'s']  arbre_sujet
let%test _ = appartient_arbre ['l';'o';'n';'g']  arbre_sujet
let%test _ = not (appartient_arbre ['t';'o';'t';'o'] arbre_sujet)
let%test _ = not (appartient_arbre ['b';'a']  arbre_sujet)
let%test _ = not (appartient_arbre ['l';'o';'n']  arbre_sujet)

(******************************************************************************)
(*   fonction d'ajout d'une liste éléments dans un arbre                      *)
(*   signature  : ajout : 'a list -> 'a arbre -> 'a arbre                     *)
(*   paramètres : - une liste d'éléments (caractères dans le cas d'un dico)   *)
(*                - un arbre n-aire                                           *)
(*   résultat   : l'arbre n-aire avec le mot ajouté                           *)
(******************************************************************************)
let rec ajout_arbre lc (Noeud (b, lb)) =
  match lc with
  (* on a épuisé la liste : le résultat est le noeud sur lequel on
     est arrivé avec son booléen mis à vrai *)
  | [] -> Noeud (true, lb)
  (* sinon on cherche l'arbre arbre_c de la branche correspondant
     au premier caractère de la liste;
     si on ne le trouve pas, le résultat de cette recherche est un arbre
     avec une liste de branches vide.

     Le résultat de aux est le noeud en paramètre
     que l'on met à jour en remplacant dans sa liste de branches,
     la branche du premier caractère par la branche dont l'arbre est
     le résultat de l'ajout du reste des caractères à l'arbre arbre_c *)
  | c::qlc ->
     let arbre_c =
       let l = recherche c lb in
       match l with
       | None   -> Noeud (false, [])
       | Some a -> a
     in Noeud (b, maj c (ajout_arbre qlc arbre_c) lb)

let arbre_sujet2 =
  List.fold_right ajout_arbre
    [['b';'a';'s']; ['b';'a';'t']; ['d';'e']; ['l';'a']; ['l';'a';'i'];
     ['l';'a';'i';'d']; ['l';'a';'i';'t']; ['l';'a';'r';'d']; ['l';'e'];
     ['l';'e';'s']; ['l';'o';'n';'g']]
    (Noeud (false,[]))

let arbre_sujet3 =
  List.fold_right ajout_arbre
    [['b';'a';'s']; ['l';'a';'i';'t']; ['b';'a';'t']; ['l';'e']; ['d';'e'];
     ['l';'a';'i']; ['l';'a';'i';'d']; ['l';'e';'s']; ['l';'a';'r';'d'];
     ['l';'a']; ['l';'o';'n';'g']]
    (Noeud (false,[]))

let%test _ = arbre_sujet2 = arbre_sujet
let%test _ = arbre_sujet3 = arbre_sujet

(*********************************************************************************)
(*   fonction de retrait qui enlève un élément d'un arbre n-aire.                *)
(*   signature  : retrait_arbre : 'a list -> 'a arbre -> 'a arbre                *)
(*   paramètres : - elt : élément de l'arbre à retirer                           *)
(*                - arbre : arbre dans lequel retirer l'élément                  *)
(*   résultat   : l'arbre n-aire sans l'élement s'il existe, sinon le même arbre *)
(*********************************************************************************)
let rec retrait_arbre elt (Noeud(fin, bl)) =
   match elt with
      | [] -> Noeud(false, bl)
      | h::q -> match recherche h bl with 
               | None -> Noeud(fin, bl)
               | Some newNode -> Noeud(fin, maj h (retrait_arbre q newNode) bl)


let%test _ =  retrait_arbre [] arbre_sujet = arbre_sujet
let%test _ =  retrait_arbre [] arbre_sujet2 = arbre_sujet2
let%test _ =  retrait_arbre [] arbre_sujet3 = arbre_sujet3


let%test _ =  retrait_arbre ['c';'o';'c';'a'] arbre_sujet = arbre_sujet
let%test _ =  retrait_arbre ['c';'o';'c';'a'] arbre_sujet2 = arbre_sujet2
let%test _ =  retrait_arbre ['c';'o';'c';'a'] arbre_sujet3 = arbre_sujet3

let%test _ =  not (appartient_arbre ['b';'a';'s'] (retrait_arbre ['b';'a';'s'] arbre_sujet2))
let%test _ =  appartient_arbre ['b';'a';'t'] (retrait_arbre ['b';'a';'s'] arbre_sujet2)



(*********************************************************************************)
(*   fonction de parcours qui partcourt tout l'arbre afin de renvoyer 
   chaque mot dans l'orde dans lequel ils apparaissent dans l'arbre.
   Dans le cas de la figure un du TP4, le premier mot serait bas, puis bat,
   puis de, puis laid etc jusqu'à long                                           *)
(*   signature  : parcours_arbre : 'a arbre -> 'a list list                      *)
(*   paramètres : - arbre : arbre dans lequel afficher les éléments              *)
(*   résultat   : la liste de élement dans l'ordre lexicographique               *)
(*********************************************************************************)

let rec parcours_arbre_beta (Noeud(fin, bl)) = (*Version expérimentale (marche mais moche)*)
   match bl with
   (* Cas de base :*)
   | [] -> [[]]

   (* Je mets la lettre de la permière branche sur l'appelle récursif des fils. Le tout concatainné à l'appel récursif sur la queue des branches*)
   | (c, fils)::q -> (List.map(fun elt -> c::elt) (parcours_arbre_beta fils))@match q with
                                                                        (* S'il n'y a pas de que je renvoie liste vide pour éviter d'enregistrer tous les mots de l'arbre *)
                                                                        | [] -> [] 
                                                                        | h::q ->   if fin then 
                                                                                       []::parcours_arbre_beta (Noeud(fin, h::q)) (* Si c'est un noeud terminal alors l'ajoute la liste vire pour faire finir le mot*)
                                                                                    else parcours_arbre_beta (Noeud(fin, h::q)) (*Sinon non*)


let rec parcours_arbre (Noeud(fin, bl)) = (*Vesion propre*)
   match bl with
   (* Cas de base :*)
   | [] -> if fin then [[]] else []

   (* Je mets la lettre de la permière branche sur l'appelle récursif des fils. Le tout concatainné à l'appel récursif sur la queue des branches*)
   | (c, fils)::q -> (List.map(fun elt -> c::elt) (parcours_arbre fils))@parcours_arbre (Noeud(fin,q))
                     
let un_arbre_vide = Noeud(false,[])
let%test _ =  parcours_arbre un_arbre_vide = []
let%test _ =  arbre_sujet2 = List.fold_right ajout_arbre (parcours_arbre arbre_sujet2) (Noeud (false,[]))
let%test _ =  arbre_sujet3 = List.fold_right ajout_arbre (parcours_arbre arbre_sujet3) (Noeud (false,[]))

(* Renvoie true si tous les noeuds de l'arbre sont non terminal (à false), renvoie false sinon*)
let rec arbre_vide (Noeud(fin, bl)) =
   match bl with
   | [] -> not fin
   | (_,abr)::q -> arbre_vide abr && arbre_vide (Noeud(fin, q))


let un_arbre_vide2 = Noeud(false,[ ('c', Noeud(false, [])) ])
let un_arbre_vide3 = Noeud(false,[ ('c', Noeud(false, [])); ('o', Noeud(false, [])) ])
let un_arbre_vide4 = Noeud(false,[ ('c', Noeud(false, [('c', Noeud(false, []))])); ('o', Noeud(false, [])) ])

let un_arbre_pas_vide = Noeud(true,[])
let un_arbre_pas_vide2 = Noeud(true,[ ('c', Noeud(false, [])) ])
let un_arbre_pas_vide2_bis = Noeud(false,[ ('c', Noeud(true, [])) ])
let un_arbre_pas_vide3 = Noeud(false,[ ('c', Noeud(false, [])); ('o', Noeud(true, [])) ])
let un_arbre_pas_vide3_bis = Noeud(false,[ ('c', Noeud(true, [])); ('o', Noeud(false, [])) ])
let un_arbre_pas_vide4 = Noeud(false,[ ('c', Noeud(false, [('c', Noeud(true, []))])); ('o', Noeud(false, [])) ])
let un_arbre_pas_vide5 = Noeud(false,[ ('c', Noeud(false, [('c', Noeud(false, []))])); ('o', Noeud(true, [])) ])
let un_arbre_pas_vide6 = Noeud(false,[ ('c', Noeud(true, [])) ])


let%test _ =  arbre_vide un_arbre_vide = true
let%test _ =  arbre_vide un_arbre_vide2 = true
let%test _ =  arbre_vide un_arbre_vide3 = true
let%test _ =  arbre_vide un_arbre_vide4 = true

let%test _ =  arbre_vide un_arbre_pas_vide = false
let%test _ =  arbre_vide un_arbre_pas_vide2 = false
let%test _ =  arbre_vide un_arbre_pas_vide2_bis = false
let%test _ =  arbre_vide un_arbre_pas_vide3 = false
let%test _ =  arbre_vide un_arbre_pas_vide4 = false
let%test _ =  arbre_vide un_arbre_pas_vide5 = false
let%test _ =  arbre_vide un_arbre_pas_vide6 = false


let%test _ =  arbre_vide arbre_sujet = false
let%test _ =  arbre_vide arbre_sujet2 = false
let%test _ =  arbre_vide arbre_sujet3 = false

(*********************************************************************************)
(*   fonction d'élagage qui partcourt tout l'arbre afin de couper 
   les branches inutilse i.e. les branches dont les noeuds ne correspondent jamais à un élément *)
             
(*   signature  : parcours_arbre : 'a arbre -> 'a arbre                          *)
(*   paramètres : - arbre : arbre que l'on veut élaguer                          *)
(*   résultat   : arbre élagué                                                   *)
(*********************************************************************************)

let rec elagage (Noeud(fin, bl)) = 
      let l = List.filter (fun (_,a) -> not (arbre_vide a)) bl in 
      let l' = List.map (fun (x, a) -> (x, elagage a)) l in
      Noeud(fin, l')


let%test _ =  elagage un_arbre_vide = Noeud(false,[])
let%test _ =  elagage un_arbre_vide2 = Noeud(false,[])
let%test _ =  elagage un_arbre_vide3 = Noeud(false,[])
let%test _ =  elagage un_arbre_vide4 = Noeud(false,[])

let%test _ =  elagage un_arbre_pas_vide = un_arbre_pas_vide
let%test _ =  elagage un_arbre_pas_vide2 = Noeud(true, [])
let%test _ =  elagage un_arbre_pas_vide2_bis = Noeud(false,[ ('c', Noeud(true, [])) ])
let%test _ =  elagage un_arbre_pas_vide3 = Noeud(false,[('o', Noeud(true, [])) ])
let%test _ =  elagage un_arbre_pas_vide3_bis = Noeud(false,[('c', Noeud(true, [])) ])
let%test _ =  elagage un_arbre_pas_vide4 = Noeud(false,
                                             [ ('c', Noeud(false, [('c', Noeud(true, []))]))])
let%test _ =  elagage un_arbre_pas_vide5 = Noeud(false,
                                             [ ('o', Noeud(true, [])) ])
let%test _ =  elagage un_arbre_pas_vide6 = un_arbre_pas_vide6




let%test _ =  elagage (retrait_arbre ['d';'e'] arbre_sujet) <> arbre_sujet


