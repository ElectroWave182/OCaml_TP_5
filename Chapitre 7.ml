Exercice 1 :
------------


1)

1.1)

fonction '_a * '_a list -> '_a


let rec minNonVide = fun

(* Cas récursif *)
(petit, tete :: queue) ->
	if tete < petit
	then minNonVide (queue, tete)
	else minNonVide (queue, petit)

(* Cas de base [] *)
| (mini, _) -> mini
;;


1.2)

fonction '_a list -> '_a


let min = fun

[] -> failwith "La liste est vide."

| (tete :: queue) ->
	minNonVide (queue, tete)
;;


min (["4" ; "-2" ; "6" ; "-42" ; "36"]) ;;
-> "-2"

min ([`4` ; `2` ; `6` ; `-` ; `3`]) ;;
-> `-`

min ([4 ; -2 ; 6 ; -42 ; 36]) ;;
-> -42

min ([0]) ;;
-> 0

min ([]) ;;
-> Erreur


2)

fonction '_a * '_a list -> '_a list


let rec enleve = fun

(element, tete :: queue) ->
	if tete = element
	(* Cas de base élément trouvé *)
	then queue
	(* Cas récursif *)
	else tete :: enleve (element, queue)

(* Cas d'erreur [] *)
| _ -> failwith "L'élément à enlever n'est pas dans la liste."
;;


enleve ("-2", ["4" ; "-2" ; "6" ; "-42" ; "36"]) ;;
-> ["4" ; "6" ; "-42" ; "36"]

enleve (`2`, [`4` ; `2` ; `6` ; `-` ; `3`]) ;;
-> [`4` ; `6` ; `-` ; `3`]

enleve (-2, [4 ; -2 ; 6 ; -42 ; 36]) ;;
-> -2

enleve (-3, [4 ; -2 ; 6 ; -42 ; 36]) ;;
-> Erreur

enleve (0, [0]) ;;
-> []

enleve (0, []) ;;
-> Erreur


3)

fonction '_a list -> '_a list


let rec naif = fun

(* Cas de base *)
[] -> []

(* Cas récursif *)
| liste ->
	let mini = min (liste)
	in
		let nouvListe = enleve (mini, liste)
		in
			mini :: naif (nouvListe)
;;


naif (["4" ; "-2" ; "6" ; "-42" ; "36"]) ;;
-> ["-2" ; "-42" ; "36" ; "4" ; "6"]

naif ([`4` ; `2` ; `6` ; `-` ; `3`]) ;;
-> [`-` ; `2` ; `3` ; `4` ; `6`]

naif ([4 ; -2 ; 6 ; -42 ; 36]) ;;
-> [-42 ; -2 ; 4 ; 6 ; 36]

naif ([1]) ;;
-> [1]

naif ([]) ;;
-> []



Exercice 2 :
------------


1)

fonction '_a * '_a list -> '_a list


let rec insertion = fun

(element, tete :: queue) ->
	if element > tete
	(* Cas récursif *)
	then tete :: insertion (element, queue)
	(* Cas de base milieu *)
	else element :: tete :: queue

(* Cas de base fin *)
| (element, _) -> [element]
;;


insertion ("-3", ["-2" ; "-42" ; "36" ; "4" ; "6"]) ;;
-> ["-2" ; "-3" ; "-42" ; "36" ; "4" ; "6"]

insertion (`5`, [`-` ; `2` ; `3` ; `4` ; `6`]) ;;
-> [`-` ; `2` ; `3` ; `4` ; `5` ; `6`]

insertion (0, [-42 ; -2 ; 4 ; 6 ; 36]) ;;
-> [-42 ; -2 ; 0 ; 4 ; 6 ; 36]

insertion (1, [0]) ;;
-> [0 ; 1]

insertion (-1, [0]) ;;
-> [-1 ; 0]

insertion (0, []) ;;
-> [0]


2)

fonction '_a list -> '_a list


let rec tri_insere = fun

(* Cas récursif *)
(tete :: queue) ->
	let appel = tri_insere (queue)
	in
		insertion (tete, appel)

(* Cas de base [] *)
| _ -> []
;;


tri_insere (["4" ; "-2" ; "6" ; "-42" ; "36"]) ;;
-> ["-2" ; "-42" ; "36" ; "4" ; "6"]

tri_insere ([`4` ; `2` ; `6` ; `-` ; `3`]) ;;
-> [`-` ; `2` ; `3` ; `4` ; `6`]

tri_insere ([4 ; -2 ; 6 ; -42 ; 36]) ;;
-> [-42 ; -2 ; 4 ; 6 ; 36]

tri_insere ([1]) ;;
-> [1]

tri_insere ([]) ;;
-> []



Exercice 3 :
------------


1)

1.1)

fonction '_a list -> int


let rec longueur = fun

(* Cas récursif *)
(_ :: queue) ->
	longueur (queue) + 1

(* Cas de base *)
| _ -> 0
;;


1.2)

fonction '_a list * int -> '_a list * '_a list


let rec diviseRec = fun

(* Cas de base *)
(fin, 0) ->
    [], fin

(* Cas récursif *)
| (tete :: queue, tailleMorceau) ->
    let appel = diviseRec (queue, tailleMorceau - 1)
    in
        tete :: fst (appel), snd (appel)
;;


1.3)

fonction '_a list -> '_a list * '_a list


let divise = fun

liste ->
    let taille = longueur (liste)
    in
        if taille < 2
        then failwith "La liste a moins de 2 éléments."
        else diviseRec (liste, taille / 2)
;;


divise ([4 ; -2 ; 6 ; -42 ; 36]) ;;
-> [-42 ; -2], [4 ; 6 ; 36]

divise ([4 ; -2 ; -42 ; 36]) ;;
-> [4 ; -2], [-42 ; 36]

divise ([1]) ;;
-> Erreur

divise ([]) ;;
-> Erreur


2)

fonction '_a list * '_a list -> '_a list


let rec fusion = fun

(* Cas récursif *)
(tete_a :: queue_a, tete_b :: queue_b) ->
    if tete_a < tete_b
    then tete_a :: fusion (queue_a, tete_b :: queue_b)
    else tete_b :: fusion (tete_a :: queue_a, queue_b)

(* Cas de base *)
| ([], liste) -> liste
| (liste, []) -> liste
;;


fusion ([-4 ; -3 ; 2], [-5 ; -2 ; 0 ; 36]) ;;
-> [-5; -4; -3; -2; 0; 2; 36]

fusion ([-4 ; -3 ; 2], []) ;;
-> [-4 ; -3 ; 2]

fusion ([], [-5 ; -2 ; 0 ; 36]) ;;
-> [-5 ; -2 ; 0 ; 36]

fusion ([], []) ;;
-> []


3)

fonction '_a list -> '_a list


let rec tri_fusion = fun

(* Cas de base *)
[] -> []
| [element] -> [element]

(* Cas récursif *)
| liste ->
    (* Diviser *)
    let appel = divise (liste)
    in
        let debut = fst (appel)
        and fin = snd (appel)
        in
            (* Régner *)
            let debutTrie = tri_fusion (debut)
            and finTrie = tri_fusion (fin)
            in
                (* Combiner *)
                fusion (debutTrie, finTrie)
;;


tri_fusion (["4" ; "-2" ; "6" ; "-42" ; "36"]) ;;
-> ["-2" ; "-42" ; "36" ; "4" ; "6"]

tri_fusion ([`4` ; `2` ; `6` ; `-` ; `3`]) ;;
-> [`-` ; `2` ; `3` ; `4` ; `6`]

tri_fusion ([4 ; -2 ; 6 ; -42 ; 36]) ;;
-> [-42 ; -2 ; 4 ; 6 ; 36]

tri_fusion ([1]) ;;
-> [1]

tri_fusion ([]) ;;
-> []



Exercice 4 :
------------


1)

fonction '_a list -> '_a list * bool


let rec parcours = fun

(tete :: cou :: queue) ->
    if tete > cou

    (* Cas récursif d'échange *)
    then
        let appel = parcours (tete :: queue)
        in
            let listeFin = fst (appel)
            in
                (cou :: listeFin, true)

    (* Cas récrusif de non échange *)
    else
        let appel = parcours (cou :: queue)
        in
            let listeFin = fst (appel)
            and echange = snd (appel)
            in
                (tete :: listeFin, echange)

(* Cas de base *)
| [dernier] ->
    ([dernier], false)

(* Cas élémentaire : [] *)
| _ ->
    ([], false)
;;


parcours (["4" ; "-2" ; "6" ; "-42" ; "36"]) ;;
-> ["-2" ; "4" ; "-42" ; "36" ; "6"], true

parcours ([`4` ; `2` ; `6` ; `-` ; `3`]) ;;
-> [`2` ; `4` ; `-` ; `3` ; `6`], true

parcours ([4 ; -2 ; 6 ; -42 ; 36]) ;;
-> [-2 ; 4 ; -42 ; 6 ; 36], true

parcours ([-42 ; -2 ; 4 ; 6 ; 36]) ;;
-> [-42 ; -2 ; 4 ; 6 ; 36], false

parcours ([1]) ;;
-> [1], false

parcours ([]) ;;
-> [], false


2)

fonction '_a list -> '_a list


let rec tri_bulle = fun

liste ->
    let resultat = parcours (liste)
    in
        let parcourue = fst (resultat)
        and continuer = snd (resultat)
        in
            if continuer

            (* Cas récursif *)
            then tri_bulle (parcourue)

            (* Cas de base : liste triée *)
            else parcourue
;;


tri_bulle (["4" ; "-2" ; "6" ; "-42" ; "36"]) ;;
-> ["-2" ; "-42" ; "36" ; "4" ; "6"]

tri_bulle ([`4` ; `2` ; `6` ; `-` ; `3`]) ;;
-> [`-` ; `2` ; `3` ; `4` ; `6`]

tri_bulle ([4 ; -2 ; 6 ; -42 ; 36]) ;;
-> [-42 ; -2 ; 4 ; 6 ; 36]

tri_bulle ([1]) ;;
-> [1]

tri_bulle ([]) ;;
-> []



Exercice 5 :
------------


1)

fonction '_a * '_a list -> '_a list * '_a list


let rec partition = fun

(* Cas récursif *)
(pivot, tete :: queue) ->
    let appel = partition (pivot, queue)
    in
        let domainePetit = fst (appel)
        and domaineGrand = snd (appel)
        in
            if tete < pivot
            then (tete :: domainePetit, domaineGrand)
            else (domainePetit, tete :: domaineGrand)

(* Cas de base : [] *)
| _ -> [], []
;;


partition (("4", ["-2" ; "6" ; "-42" ; "36"])) ;;
-> ["-2" ; "-42" ; "36"], ["6"]

partition ((`4`, [`2` ; `6` ; `-` ; `3`])) ;;
-> [`2` ; `-` ; `3`], [`6`]

partition ((4, [-2 ; 6 ; -42 ; 36])) ;;
-> [-2 ; -42], [6 ; 36]

partition (1, [0]) ;;
-> [0], []

partition (1, [2]) ;;
-> [], [2]

partition (0, []) ;;
-> [], []


2)

fonction '_a list -> '_a list


let rec quick = fun


(* Cas récursif *)
(pivot :: queue) ->

    (* Diviser *)
    let resultat = partition (pivot, queue)
    in
        let domainePetit = fst (resultat)
        and domaineGrand = snd (resultat)
        in

            (* Régner *)
            let petiteTriee = quick (domainePetit)
            and grandeTriee = quick (domaineGrand)
            in

                (* Combiner *)
                petiteTriee @ pivot :: grandeTriee


(* Cas de base : [] *)
| _ -> []
;;


quick (["4" ; "-2" ; "6" ; "-42" ; "36"]) ;;
-> ["-2" ; "-42" ; "36" ; "4" ; "6"]

quick ([`4` ; `2` ; `6` ; `-` ; `3`]) ;;
-> [`-` ; `2` ; `3` ; `4` ; `6`]

quick ([4 ; -2 ; 6 ; -42 ; 36]) ;;
-> [-42 ; -2 ; 4 ; 6 ; 36]

quick ([1]) ;;
-> [1]

quick ([]) ;;
-> []



Exercice 6 :
------------


let annuaire = [
    ("Claude", 1785) ;
    ("Andrée", 6949) ;
    ("Antoine", 1386) ;
    ("Françoise", 2638) ;
    ("Pascal", 1009) ;
    ("Jean", 2066) ;
    ("Ginette", 5250) ;
    ("Julien", 8043) ;
    ("Pierre", 4773) ;
    ("Paul", 3367) ;
    ("Cécile", 5843)
] ;;


1)

1.1)

fonction string * (string * int) list -> string * int


let rec cherche = fun

(* Cas récursif *)
(cible, tete :: queue) ->
    if fst (tete) = cible
    then tete
    else cherche (cible, queue)

(* Cas de base : [] *)
| _ -> failwith "La parsonne n'est pas dans l'annuaire."
;;


cherche ("Cécile", annuaire) ;;
-> "Cécile", 5843

cherche ("Aiden", annuaire) ;;
-> Erreur

cherche ("Cécile", []) ;;
-> Erreur


1.2)

fonction string * (string * int) list -> (string * int) list


let rec supprime = fun

(* Cas récursif *)
(cible, tete :: queue) ->

    let appel = supprime (cible, queue)
    in
        if fst (tete) = cible
        then appel
        else tete :: appel

(* Cas de base : [] *)
| _ -> []
;;


supprime ("Cécile", annuaire) ;;
-> annuaire - ["Cécile", 5843]

supprime ("Aiden", annuaire) ;;
-> annuaire

supprime ("Cécile", []) ;;
-> []


2)

2.1)

fonction (string * int) * (string * int) list -> (string * int) list


let rec insere = fun

(* Cas récursif *)
(element, tete :: queue) ->
    if fst (element) < fst (tete)
    then element :: tete :: queue
    else tete :: insere (element, queue)

(* Cas de base : [] *)
| (element, _) ->
    [element]
;;


insere (("Shawn", 4269), annuaire) ;;
-> annuaire + ["Shawn", 4269]

insere (("Shawn", 4269), []) ;;
-> ["Shawn", 4269]


2.2)

fonction (string * int) list -> (string * int) list


let rec tri_insertion = fun

(* Cas récursif *)
(tete :: queue) ->
    insere (tete, tri_insertion (queue))

(* Cas de base : [] *)
| _ -> []
;;


tri_insertion (annuaire) ;;
-> [
    "Andrée", 6949 ;
    "Antoine", 1386 ;
    "Cécile", 5843 ;
    "Claude", 1785 ;
    "Françoise", 2638 ;
    "Ginette", 5250 ;
    "Jean", 2066 ;
    "Julien", 8043 ;
    "Pascal", 1009 ;
    "Paul", 3367 ;
    "Pierre", 4773
]

tri_insertion ([]) ;;
-> []


3)

3.1)


fonction string -> int


let long = fun

chaine ->
    string_length chaine
;;


long ("Xavier") ;;
-> 6

long ("") ;;
-> 0


3.2)

fonction string -> int


let clef = fun

chaine ->
    long (chaine) mod 4
;;


clef ("Xavier") ;;
-> 2

clef ("") ;;
-> 0


3.3)

3.3.1)

fonction int * '_a * '_a list list -> '_a list list


let rec ajouteNpositif = fun

(* Cas de base : indice atteint *)
(0, element, listeTete :: listesQueue) ->
    (element :: listeTete) :: listesQueue

(* Cas récursif *)
| (indice, element, listeTete :: listesQueue) ->

    let appel = ajouteNpositif (indice - 1, element, listesQueue)
    in
        listeTete :: appel

(* Cas d'erreur *)
| _ -> failwith "Indice d'ajout en dehors des limites de la table"
;;


3.3.2)

fonction int * '_a * '_a list list -> '_a list list


let ajouteN = fun

(indice, element, table) ->
    if indice < 0
    then failwith "Indice d'ajout négatif"
    else ajouteNpositif (indice, element, table)
;;


ajouteN (1, "Jude", [["Byron" ; "Paul"] ; ["Axel" ; "Mark" ; "Bobby"] ; [] ; []]) ;;
-> ["Jude" ; "Axel" ; "Mark" ; "Bobby"]

ajouteN (4, "Jude", [["Byron" ; "Paul"] ; ["Axel" ; "Mark" ; "Bobby"] ; [] ; []]) ;;
-> Erreur

ajouteN (-1, "Jude", [["Byron" ; "Paul"] ; ["Axel" ; "Mark" ; "Bobby"] ; [] ; []]) ;;
-> Erreur

ajouteN (0, "", [[]]) ;;
-> [[""]]


3.4)

fonction '_a * '_a list list -> '_a list list


let ajoute = fun

(element, table) ->

    let indice = clef (fst (element))
    in
        ajouteN (indice, element, table)
;;


(*
let ajoute = fun

((nom, valeur), table) ->

    let indice = clef (nom)
    in
        ajouteN (indice, (nom, valeur), table)

| (element, table) ->
    let indice = clef (element)
    in
        ajouteN (indice, element, table)
;;
*)


ajoute (("Nathan"), [["Byron" ; "Paul"] ; ["Axel" ; "Mark" ; "Bobby"] ; [] ; []]) ;;

ajoute ("", [[]]) ;;
-> [[""]]


3.5)

fonction (string * int) list -> (string * int) list list


let rec hachage = fun

(* Cas récursif *)
(tete :: queue) ->
    ajoute (tete, hachage (queue))

(* Cas de base : [] *)
| _ ->
    [[] ; [] ; [] ; []]
;;


hachage (annuaire) ;;
-> [
    [
        "Jean", 2066 ;
        "Paul", 3367
    ] ; [
        "Françoise", 2638
    ] ; [
        "Claude", 1785 ;
        "Andrée", 6949 ;
        "Pascal", 1009 ;
        "Julien", 8043 ;
        "Pierre", 4773 ;
        "Cécile", 5843
    ] ; [
        "Antoine", 1386 ;
        "Ginette", 5250
    ]
]

hachage ([]) ;;
-> [[] ; [] ; [] ; []]


let tableVide = hachage ([]) ;;
let tableAnn = hachage (annuaire) ;;


3.6)

3.6.1)

fonction string * (string * int) list -> bool


let rec rechercheListe = fun

(cible, tete :: queue) ->

    (* Cas de base : personne trouvée *)
    fst (tete) = cible

    (* Cas récursif *)
    or rechercheListe (cible, queue)

(* Cas de base : personne introuvable *)
| _ -> false
;;


3.6.2)

fonction int * string * (string * int) list list -> bool


let rec rechercheTableRec = fun

(* Cas de base : clef trouvée *)
(0, nom, listeTete :: _) ->
    rechercheListe (nom, listeTete)

(* Cas récursif *)
| (indice, nom, listeTete :: listesQueue) ->
    rechercheTableRec (indice - 1, nom, listesQueue)

(* Cas d'erreur *)
| _ -> failwith "La création de clefs n'est pas coordonnée avec la table de hachage."
;;


3.6.3)

fonction string * (string * int) list list -> bool


let rechercheTable = fun

(nom, table) ->

    let clefCible = clef (nom)
    in
        rechercheTableRec (clefCible, nom, table)
;;


rechercheTable ("Cécile", tableAnn) ;;
-> true

rechercheTable ("Paolo", tableAnn) ;;
-> false

rechercheTable ("Cécile", tableVide) ;;
-> false


3.7)

3.7.1)

fonction string * (string * int) list -> bool


let rec accesListe = fun

(cible, tete :: queue) ->
    if fst (tete) = cible

    (* Cas de base : personne trouvée *)
    then snd (tete)

    (* Cas récursif *)
    else accesListe (cible, queue)

(* Cas d'erreur *)
| _ -> failwith "La personne n'est pas dans la table de hachage."
;;


3.6.2)

fonction int * string * (string * int) list list -> bool


let rec rechercheValeurRec = fun

(* Cas de base : clef trouvée *)
(0, nom, listeTete :: _) ->
    accesListe (nom, listeTete)

(* Cas récursif *)
| (indice, nom, listeTete :: listesQueue) ->
    rechercheValeurRec (indice - 1, nom, listesQueue)

(* Cas d'erreur *)
| _ -> failwith "La création de clefs n'est pas coordonnée avec la table de hachage."
;;


3.7.3)

fonction string * (string * int) list list -> bool


let rechercheValeur = fun

(nom, table) ->

    let clefCible = clef (nom)
    in
        rechercheValeurRec (clefCible, nom, table)
;;


rechercheValeur ("Cécile", tableAnn) ;;
-> 5843

rechercheValeur ("Paolo", tableAnn) ;;
-> Erreur

rechercheValeur ("Cécile", tableVide) ;;
-> Erreur
