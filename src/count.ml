type elem = Seq of string | Elem of string 
type reference = string

type component =  Call of reference | Cons of int * elem list

type rule = string * component list

type grammar = rule list

let number_of_recursions (comp:component) = 
	match comp with
		| Call(_) -> 0
		| Cons(n, l) -> List.length l

let number_of_nodes (comp:component) = 
	match comp with
		| Call(_) -> 0
		| Cons(n, l) -> n
		
let equals a b = let res = ref true in 
	if Array.length a != Array.length b 
		then res := false
	else 
		begin
			let index = ref 0 in
			while !index < (Array.length a) do
				if a.(!index) != b.(!index) 
					then res := false
					else
						index := !index + 1
			done;
		end;
	!res
		
let combinatorics_numbers (comp:component) n = 
	let nb_recursions = number_of_recursions(comp) in 
		if (n < 0) || (nb_recursions == 0) then [[||]]
		else 
			begin
				let base = Array.make nb_recursions 0 in 
				let result = ref [[||]] in
				base.(0) <- n;
				while ( not(equals (List.nth !result 0) base)) do
					result := [base] @ !result;
					for i=0 to (Array.length base -2) do
						if (base.(i) -1) >= (base.(i+1) +1) then
							begin
								base.(i) <- base.(i) -1;
								base.(i+1) <- base.(i+1)+1
							end
					done
				done;
				!result
			end
			
let rec factorial n = 
	if n <= 1 then 1 
	else n * (factorial (n-1))
	
let combination base = 
	if Array.length base == 0 
		then 0
	else 
		begin
			let duplicate_numbers = ref []
			and count = ref 0 in
			for i=0 to (Array.length base -2) do
				if base.(i) == base.(i + 1) then count := !count +1
				else if !count > 0 then 
					begin
						duplicate_numbers := !duplicate_numbers @ [(!count+1)];
						count := 0
					end
			done;
			if !count > 0 then duplicate_numbers := !duplicate_numbers @ [(!count+1)];
			let result = ref (factorial(Array.length base)) in
			for i = 0 to (List.length !duplicate_numbers -1) do
				result := !result / (factorial (List.nth !duplicate_numbers i))
			done;
			!result
		end
			
let add_in_backup back_up nb e = 
	let backup = ref back_up in 
	if Array.length !backup != nb 
		then !backup.(nb) <- e
	else
		begin
			let backup_increased = Array.make (2*nb) 0 in
			for i=0 to (nb-1) do
				backup_increased.(i) <- !backup.(i);
				backup := backup_increased
			done;
		end;
	!backup

let read_file filename = 
	let lines = ref [] in
	let chan = open_in filename in
		try
  	while true; do
   	 lines := !lines @ [input_line chan] 
  	done; !lines
		with End_of_file ->
  	close_in chan;
		!lines

let rec calcul_base_aux (comp:component) backup n base = 
	match base with 
		| [] -> 0
		| hd::t -> 
			if number_of_recursions comp == 0 && 
			number_of_nodes comp == n then
				(calcul_base_aux comp backup n t) + 1
			else if number_of_nodes comp <= n then 
				begin
					let tmp = ref 1 in
						for i=0 to (Array.length hd -1) do
							tmp := !tmp * backup.(i)
						done;
					(calcul_base_aux comp backup n t) + (combination hd) * !tmp;
				end
			else
				(calcul_base_aux comp backup n t)

let calcul_base (comp:component) backup nb_elements n = 
	let base = combinatorics_numbers comp (nb_elements - (number_of_nodes comp))
	in calcul_base_aux comp backup n base	
	
let rec calcul_comp (l:component list) backup nb_elements n =
	match l with 
		| [] -> 0
		| hd::t -> ((calcul_base hd backup nb_elements n) + (calcul_comp t backup nb_elements n))
	
let rec set_backup lines backup i = 
	match lines with
	| [] -> backup
	| hd::t -> backup.(i) <- (int_of_string hd); set_backup t backup (i+1)

let get_comps_from_rule (r:rule) = 
	match r with
		| s, l -> l 

let count n (r:rule) = 
	let lines = read_file "backup.cnt" in 
	let nb_elements = ref (List.length lines) in
	let backup = ref (Array.make (max !nb_elements 1000) 0) in
		begin
			backup := set_backup lines !backup 0;
			let bn = ref 0 in  
			for i=(!nb_elements) to n do
				bn := !bn + (calcul_comp (get_comps_from_rule r) !backup i n);
				if Array.length !backup == !nb_elements then
					begin
						let backup_increased = Array.make (!nb_elements * 2) 0 in
						for i=0 to (!nb_elements-1) do
							backup_increased.(i) <- !backup.(i)
						done;
						backup := backup_increased
					end;
				!backup.(!nb_elements) <- !bn;
				nb_elements := !nb_elements +1
			done;
			!backup
		end
					
	
		
















































						
