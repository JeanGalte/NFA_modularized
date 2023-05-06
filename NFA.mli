open Set

(* /!\ Ce n'est pas une implémentation d'automate fini non déterministe classique, voir le README pour plus d'informations *)

module type MonoidalType =
(*
Comme OrderedType mais assure en plus la structure de monoïde d'un type, ça permet de faire notre alphabet.
Assure la possibilité de faire un monoïde libre et semi groupe libre sur un ensemble 
On laissera libre l'implémentation des habitants de MonoidalType
*)
sig
(* Usuellement string *)
	type t 
(* Usuellement > *)
	val compare : t -> t -> int
(* Usuellement ^ *)
	val ( + ) : t -> t -> t
(* Théoriquement epsilon, on prendra usuellement "e" puisqu'on est en ascii *)
	val e : t
end

module type Monset =
sig
	include MonoidalType
	module S : Set.S with type elt = t
end 

module type MonSetMakeT = functor (MT : MonoidalType) -> Monset 

module MonsetMake : MonSetMakeT

module type Lang =
sig
	module Sub : Monset
(* Attention, Sub.t c'est Monset.t c'est un mot, Sub.S.t c'est un ensemble de mots Set.S.t *)
	type t =
		| B of Sub.S.t
		| Star of Sub.t
		| Union of t * t
		| Prod of t * t
	val compare : t -> t -> int
end

module LangMake (M : Monset) : Lang 

(* Décrit les tages étiquetant les transitions d'un automate, voir le document en readme pour bien comprendre pourquoi c'est nécessaire *)
(*	L'implémentation de * est laissée libre, mais à ma connaissance la seule implémentation pertinente qui colle à la théorie se trouve dans le .ml, si vous en connaissez une autre n'hésitez pas à m'en faire part *)
module type SetLang = 
sig
	include Lang
	module S : Set.S with type elt = t
end

module type SetLangMakeT = functor (L : Lang) -> SetLang 

module SetLangMake : SetLangMakeT

module type EtatalType =
sig
	type t
	val compare : t -> t -> int	
	val d : t
	val f : t
end

module type SetEtat = 
sig
	include EtatalType
	module S : Set.S with type elt = t
end

module type SetEtatMakeT = functor (E : EtatalType) -> SetEtat

module SetEtatMake : SetEtatMakeT
(*
Enforce pratiquement totalement la structure du code, SetEtat implémentera au moins
struct
	include E
	include Set.Make(E)
end
Et c'est tout ce dont on a besoin
*)

module type Aut = 
sig
	type a
	type q

	type auta
	type autq
	type autt	

	type aut = { 
	alph : auta ;
	etats : autq ;
	debuts : autq ;
	fins : autq ;
	trans : autt ;
	}

	val from_lists : a list -> q list -> q list -> q list -> (q * a * q) list -> aut  
	val is_auto : auta -> autq -> autq -> autq -> autt -> bool  
	val is_auto_from_aut : aut -> bool
	val is_auto_from_lists : a list -> q list -> q list -> q list -> (q * a * q) list -> bool  
	val debuts_fins_uniques : aut -> aut 	
	(* Vérifie si un automate accepte un langage *)
end

module AutMake 
	(LS : SetLang)
	(Q : SetEtat)
	(T : Set.S with type elt = Q.S.elt * LS.S.elt * Q.S.elt) :
	Aut with type a = LS.S.elt and type q = Q.S.elt and type auta = LS.S.t and type autq = Q.S.t and type autt = T.t

