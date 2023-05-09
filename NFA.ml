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
		| Star of t
		| Union of t * t
		| Prod of t * t
		| Empty
	val compare : t -> t -> int
end

module LangMake (M : Monset) =
struct
	module Sub = M
	type t =
		| B of Sub.S.t
		| Star of t
		| Union of t * t
		| Prod of t * t
		| Empty
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
	val est_usuel : aut -> bool
	val lang_trans : aut -> q -> q -> a 
	val elimine_etat : aut -> q -> aut
	val langage_accepte : aut -> a
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

	let est_usuel (a : aut) : bool = 
		LS.S.for_all 
			(fun l -> 
				match l with
				| B ens -> LS.Sub.S.cardinal ens = 1
				| _ -> false
			) a.alph

	let lang_trans (a : aut) (e1 : q) (e2 : q) : a =
		let l = T.filter (fun (h,_,j) -> h = e1 && j = e2 ) a.trans in
		if l = T.empty then 
			LS.Empty
		else
			if T.cardinal l > 1 then 
				let () = print_string "Attention : les transitions sont étiquettées par des langages entiers dont vous pouvez faire l'union, veillez à ne pas avoir 2 transitions (e1,l1,e2) et (e1,l2,e2) mais plutôt (e1,Union l1 l2,e2). Pour plus d'information, lisez la documentation sur le readme.md"  in LS.Empty
			else 
				(fun (_,lang,_) -> lang) (T.choose l)

	let elimine_etat (a : aut) (e : q) : aut = 
		if Q.S.mem e a.debuts then let () = print_string "On ne peut pas supprimer un état de début de l'automate" in a else
		if Q.S.mem e a.fins then let () = print_string "On ne peut pas supprimer un état de fin de l'automate" in a else
		let e_dep = Q.S.filter (fun elt -> T.exists (fun (h,_,_) -> h = e) a.trans) a.etats in
		let e_fin = Q.S.filter (fun elt -> T.exists (fun (_,_,j) -> j = e) a.trans) a.etats in 
		(* Produit cartésien du pauvre. La complexité est mauvaise, mais de toute manière dans l'idéal il faudrait faire un foncteur produit cartésien en entier et l'exploiter ici*)
		let rec compact (l : 'a list list) : 'a list = 
			match l with
			| [] -> []
			| x :: xs -> x @ compact xs
		in
		let rec map_all (l : 'b list) (el : 'c)  : ('c * 'b) list =
			match l with
			| [] -> []
			| y :: ys -> (el, y) :: map_all ys el 
		in 
		let rec make_cpl (l1 : 'd list) (l2 : 'e list) : ('d * 'e) list list =
			match l1 with
			| [] -> []
			| x :: xs -> (map_all l2 x) :: (make_cpl xs l2) 
		in
		let cpls = compact (make_cpl (Q.S.elements e_fin) (Q.S.elements e_dep))
		in
		let rec ntrans_list (cpl : (Q.S.elt * Q.S.elt) list) : (T.elt list) = 
			match cpl with
			| [] -> []
			| (e1, e2) :: xs -> 
				(
					e1, 
					LS.Union
						( 
							(lang_trans a e1 e2), 
							(LS.Prod
								(
									(lang_trans a e1 e),
									(LS.Prod 
										(
										(LS.Star (lang_trans a e e)), 
										lang_trans a e e2
										)
									)
								)
							)
						)
					,
					e2
				)  
				:: (ntrans_list xs)
		in 
		let n_trans =
			T.union
				(T.filter (fun (h,_,j) -> h <> e && j <> e ) a.trans)  
				(T.of_list (ntrans_list cpls))
		in
		let n_alph =
			LS.S.union		 
				(LS.S.of_list (List.map (fun (_,l,_) -> l) (T.elements n_trans))) 
				(a.alph)
		in 
		let n_etats = Q.S.filter (fun elt -> elt <> e) a.etats
		in
		{
		alph = n_alph;
		etats = n_etats;
		debuts = a.debuts;
		fins = a.fins;
		trans = n_trans;
		}

		let langage_accepte (a : aut) : a = 
			let etats_elem_list = Q.S.elements a.etats in
			let na = debuts_fins_uniques a in
			let rec elimine_success (a : aut) (l : q list) : aut = 
				match l with
				| [] -> a
				| x :: xs -> elimine_success (elimine_etat a x) xs
			in lang_trans (elimine_success na etats_elem_list) Q.d Q.f 
end
