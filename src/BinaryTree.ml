(* Compte le nombre d'arbres binaires possible pour n noeuds *)
let comptage n =
	let pred = ref [1] in 
	let bk = ref 0 in 
	for i=(List.length !pred) to n do 
		for k=0 to (List.length !pred)-1 do
			bk := !bk + (List.nth !pred k) * (List.nth !pred ((List.length !pred)-1-k)) 
			done;	
		pred := !pred @ [!bk];
		print_int !bk;
		print_newline();
		bk := 0; 
		done;
	(List.nth !pred n);;

(* Génère aléatoirement de maière uniforme un arbre binaire de n noeuds (arbre sous forme de String)*) 
let rec gen n = match n with
	| 0 -> "0"
	| _ ->let r = ref (Random.int (comptage n)) in 
				let k = ref 0 in 
				while !r >= 0 do
					r := !r - (comptage !k) * (comptage (n-(!k)-1));
					k := !k +1; 
					done;
				k := !k -1;
				"(*, " ^ (gen !k) ^ ", " ^(gen(n-(!k)-1)) ^")";;

(*gen 3;;*)