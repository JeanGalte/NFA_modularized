open Set

module type MonoidalType =
(*
Comme OrderedType mais assure en plus la structure de monoïde d'un type, ça permet de faire notre alphabet.
Assure la possibilité de faire un monoïde libre et semi groupe libre sur un ensemble 
On laissera libre l'implémentation des habitants de MonoidalType
*)
sig
	type t
	val compare : t -> t -> int
	val ( + ) : t -> t -> t
	val e : t
end

module type EtatalType =
sig
	type t
	val compare : t -> t -> int	
	(* Valeur de début et fin réservées (on en a besoin pour trouver le langage accepté lors quand on procède par élimination d'états) *)
	val d : t
	val f : t
	val est_debut : t -> bool
	val est_fin : t -> bool
end

module type MonSet =
sig
	type x
	include Set.S with type elt = x
	val ( + ) : x -> x -> x
	val e : x
end

module type EtSet =
sig
	type x
	include Set.S with type elt = x
	val d : x
	val f : x
	val est_debut : x -> bool
	val est_fin : x -> bool
end

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
	val est_chemin : aut -> a list  -> q -> q -> bool
	(* Teste si un chemin mène d'un état à un autre*)
	val accepte : aut -> a list -> bool
end

module type MonoidalTypeMake = functor (T : OrderedType) -> MonoidalType with type t = T.t

module type EtatalTypeMake = functor (T : OrderedType) -> EtatalType with type t = T.t

module type MonSetMake = functor (MT : MonoidalType) -> 
	MonSet with type x = MT.t 

module type EtatSetMake = functor (ET : EtatalType) -> 
	EtSet with type x = ET.t 

module type AutMake = functor
	(A : MonSet)
	(Q : EtSet)
	(D : EtSet with type x = Q.x)
	(F : EtSet with type x = Q.x)
	(T : Set.S with type elt = Q.x * A.x * Q.x) -> 
	Aut with type a = A.x and type q = Q.x  and type auta = A.t and type autq = Q.t and type autt = T.t

module MonSetMake (MT : MonoidalType) : MonSet with type x = MT.t 

module EtatSetMake (ET : EtatalType) : EtSet with type x = ET.t

module AutMake 
	(A : MonSet)
	(Q : EtSet)
	(D : EtSet with type x = Q.x)
	(F : EtSet with type x = Q.x)
	(T : Set.S with type elt = Q.x * A.x * Q.x) :
	Aut with type a = A.x and type q = Q.x  and type auta = A.t and type autq = Q.t and type autt = T.t 
