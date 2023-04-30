open Set

(* Types des modules et foncteurs utilisés. Les types de foncteurs n'étaient en réalité pas obligatoire, mais ils ont permis d'enforcer certaines propriété via les types *)

module type MonoidalType =
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
	val est_chemin :  aut -> a list -> q -> q  -> bool
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
	(T : Set.S with type elt = Q.x * A.elt * Q.x) -> 
	Aut with type a = A.elt and type q = Q.x  and type auta = A.t and type autq = Q.t and type autt = T.t

module MonSetMake (MT : MonoidalType) =
struct
	type x = MT.t
	include Set.Make(MT)
	let e = MT.e
	let ( + ) = MT.( + )
end 

module EtatSetMake (ET : EtatalType) =
struct
	type x = ET.t
	include Set.Make(ET)
	let d = ET.d
	let f = ET.f
	let est_debut = ET.est_debut
	let est_fin = ET.est_fin
end

module AutMake 
	(A : MonSet)
	(Q : EtSet)
	(D : EtSet with type x = Q.x)
	(F : EtSet with type x = Q.x)
	(T : Set.S with type elt = Q.x * A.elt * Q.x) =
struct
	type a = A.elt
	type q = Q.x

	type auta = A.t
	type autq = Q.t
	type autt = T.t

	type aut = { 
	alph : auta ;
	etats : autq ;
	debuts : autq ;
	fins : autq ;
	trans : autt ;
	}

	let from_lists (l : a list) (et : q list) (d : q list) (f : q list) (t : (q * a * q) list ) : aut = 
		{
		alph = A.of_list l;
		etats = Q.of_list et;
		debuts = Q.of_list d;
		fins = Q.of_list f;
		trans = T.of_list t;
		}

	let is_auto (m : auta) (et : autq) (d : autq) (f : autq) (t : autt) : bool =
		m <> A.empty &&
		et <> Q.empty &&
		d <> Q.empty &&
		f <> Q.empty &&
		Q.for_all (fun elt -> Q.mem elt et) d &&
		Q.for_all (fun elt -> Q.mem elt et) f &&
		T.for_all 
			(fun elt ->
			match elt with
				| (j,o,h) -> Q.mem j et && Q.mem h et && A.mem o m)
			t 

	let is_auto_from_aut (a : aut) : bool =
		is_auto a.alph a.etats a.debuts a.fins a.trans

	let is_auto_from_lists (l : a list) (et : q list) (d : q list) (f : q list) (t : (q * a * q) list ) : bool =
		is_auto_from_aut (from_lists l et d f t) 

	let debuts_fins_uniques (a : aut) : aut =
		if is_auto_from_aut a then
			let nalph = A.union a.alph (A.singleton A.e) in
			let ndebuts = Q.singleton Q.d in 
			let nfins = Q.singleton Q.f in 
			let netats = Q.union (Q.union ndebuts nfins) a.etats in
			let ntrans = 
			T.union
				(
					T.union 
				 			( 
				 				T.map 
				 					(fun elt -> 
				 					match elt with
				 					| (j,o,h) -> (Q.d,A.e,j)
				 					)
				 					(T.filter 
				 						(fun elt ->
				 						match elt with
				 						| (j,o,h) -> Q.est_debut j)
				 					a.trans)
				 			) 
				 			( 
				 				T.map 
				 					(fun elt -> 
				 					match elt with
				 					| (j,o,h) -> (h, A.e, Q.f)
				 					)
				 					(T.filter 
				 						(fun elt ->
				 						match elt with
				 						| (j,o,h) -> Q.est_fin h)
				 						a.trans)
				 			)
				 	) 
				a.trans
			in
			{
			alph = nalph ;
			etats = netats ;
			debuts = ndebuts;
			fins = nfins ;
			trans = ntrans ;
			}
		else 
			a		 

		let rec est_chemin (a : aut) (l : a list)  (q1 : q) (q2 : q) : bool =
			match l with
			| x :: xs -> 
				T.fold 	
					(fun elt b -> 
					match elt with
					| (_,_,h) -> est_chemin a xs h q2  || b
					)
					(T.filter 		
						(fun elt ->
					 		match elt with
					 		| (j,o,h) -> j = q1 && o = x
					 	)
					 	a.trans)
					false
			| [] -> T.mem (q1, A.e, q2) a.trans

		let rec accepte (a : aut) (l : a list) : bool =
			if a.debuts = Q.empty then 
				false 
			else 
				let elem = Q.choose a.debuts in
					if Q.exists (est_chemin a l elem) a.fins then 
						true
					else 
						accepte {alph = a.alph; etats = a.etats; debuts = Q.filter (fun elt -> elt <> elem) a.debuts ; fins = a.fins; trans = a.trans} l
end
