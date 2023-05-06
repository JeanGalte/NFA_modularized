open Set

(* /!\ Ce n'est pas une implémentation d'automate fini non déterministe classique, voir le README pour plus d'informations *)

module type MonoidalType =
sig
	type t 
	val compare : t -> t -> int
	val ( + ) : t -> t -> t
	val e : t
end

module type Monset =
sig
	include MonoidalType
	module S : Set.S with type elt = t
end 

module type MonSetMakeT = functor (MT : MonoidalType) -> Monset 

module MonsetMake : MonSetMakeT =
functor (MT : MonoidalType) ->
struct
	include MT
	module S = Set.Make(MT) 
end

module type Lang =
sig
	module Sub : Monset
	type t =
		| B of Sub.S.t
		| Star of Sub.t
		| Union of t * t
		| Prod of t * t
	val compare : t -> t -> int
end

module LangMake (M : Monset) =
struct
	module Sub = M
	type t =
		| B of Sub.S.t
		| Star of Sub.t
		| Union of t * t
		| Prod of t * t
	let compare = compare
end

module type SetLang = 
sig
	include Lang
	module S : Set.S with type elt = t
end

module type SetLangMakeT = functor (L : Lang) -> SetLang 

module SetLangMake : SetLangMakeT  =
functor (L : Lang) ->
struct
	include L
	module S = Set.Make(L) 
end 

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

module SetEtatMake : SetEtatMakeT =
functor (E : EtatalType) ->
struct
	include E
	module S = Set.Make(E)
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
end

module AutMake 
	(LS : SetLang)
	(Q : SetEtat)
	(T : Set.S with type elt = Q.S.elt * LS.S.elt * Q.S.elt) =
struct
	type a = LS.S.elt
	type q = Q.S.elt

	type auta = LS.S.t
	type autq = Q.S.t
	type autt = T.t

	type aut = { 
		alph : auta ;
		etats : autq ;
		debuts : autq ;
		fins : autq ;
		trans : autt ;
	}

	let from_lists (la : a list) (lq : q list) (ld : q list) (lf : q list) (lt : (q * a * q) list) : aut =
		{alph = LS.S.of_list la;
		etats = Q.S.of_list lq;
		debuts = Q.S.of_list ld;
		fins = Q.S.of_list lf;
		trans = T.of_list lt;}

	let is_auto (rs : auta) (qs : autq) (ds : autq) (fs : autq) (ts : autt) : bool =
		rs <> LS.S.empty &&
		qs <> Q.S.empty &&
		ds <> Q.S.empty &&
		fs <> Q.S.empty && 
		ts <> T.empty &&
		Q.S.for_all (fun e -> Q.S.mem e qs) ds && 
		Q.S.for_all (fun e -> Q.S.mem e qs) fs &&
		T.for_all (fun (h,j,k) -> Q.S.mem h qs && Q.S.mem k qs && LS.S.mem j rs ) ts

	let is_auto_from_aut (a : aut) : bool =
		is_auto a.alph a.etats a.debuts a.fins a.trans

	let is_auto_from_lists (l : a list) (et : q list) (d : q list) (f : q list) (t : (q * a * q) list ) : bool =
		is_auto_from_aut (from_lists l et d f t) 

	let debuts_fins_uniques (a : aut) : aut =
		if is_auto_from_aut a then
			let eps = LS.B (LS.Sub.S.singleton LS.Sub.e) in
			let nalph = LS.S.union a.alph (LS.S.singleton eps) in
			let ndebuts = Q.S.singleton Q.d in 
			let nfins = Q.S.singleton Q.f in 
			let netats = Q.S.union (Q.S.union ndebuts nfins) a.etats in
			let ntrans = 
			T.union
				(
					T.union 
				 			( 
				 				T.map 
				 					(fun (j,_,_) -> (Q.d,  eps ,j))
				 					(T.filter 
				 						(fun (j,_,_) -> Q.S.mem j a.debuts)
				 					a.trans
				 					)
				 			) 
				 			( 
				 				T.map 
				 					(fun (_,_,h) -> (h, eps , Q.f))
				 					(T.filter 
				 						(fun (_,_,h) -> Q.S.mem h a.fins)
				 						a.trans
				 					)
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
end
