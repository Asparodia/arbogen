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
		
			
let rec factorial n = 
	if n <= 1 then 1 
	else n * (factorial (n-1))
	
let next_partition_with_factor base = 
	let modification = ref false and
	duplicate = ref 1 and
	den = ref 1 in 
		for i=0 to (Array.length base -2) do
			if (not (!modification)) && base.(i)-1 >= base.(i+1) +1 then
				begin
					base.(i) <- base.(i)-1;
					base.(i+1) <- base.(i+1)+1;
					modification := true
				end;
			if base.(i) == base.(i+1) then 
				begin
					incr duplicate;
					den := !den * !duplicate
				end
			else
				duplicate := 1
		done;
	!modification, factorial (Array.length base) /(!den), base
			
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
	
let rec set_backup lines backup i = 
	match lines with
	| [] -> backup
	| hd::t -> backup.(i) <- (int_of_string hd); set_backup t backup (i+1)

let get_comps_from_rule (r:rule) = 
	match r with
		| s, l -> l 

let rec calcul_comp (l:component list) backup n bn = match l with	
	| [] -> !bn
	| hd::t -> 
		if n - number_of_nodes hd >= 0 then
			if number_of_recursions hd == 0 && number_of_nodes hd == n then incr bn
			else 
				begin
					let base = ref (Array.make (max 1 (number_of_recursions hd)) 0) in
					let modification = ref true in
					!base.(0) <- (n - number_of_nodes hd);
					let factor = ref 1 in 
					if number_of_nodes hd != n then factor := number_of_recursions hd;
					while !modification do
						let product = ref 1 in
						for i=0 to (Array.length !base -1) do
							product := !product * backup.(!base.(i))
						done;
						bn := !bn + !product * !factor;
						let a, b, c = next_partition_with_factor !base in
						modification := a;
						factor := b;
						base := c 
					done
				end;
		calcul_comp t backup n bn
	 
let count n (r:rule) = 
	let lines = read_file "backup.cnt" in 
	let nb_elements = ref (List.length lines) in
	let backup = ref (Array.make (max (!nb_elements *2) 1000) 0) in
		begin
			backup := set_backup lines !backup 0; 
			for i=(!nb_elements) to n do
				let bn = calcul_comp (get_comps_from_rule r) !backup i (ref 0) in
				if Array.length !backup == !nb_elements then
					begin
						let backup_increased = Array.make (!nb_elements * 2) 0 in
						for j=0 to (!nb_elements-1) do
							backup_increased.(j) <- !backup.(j)
						done;
						backup := backup_increased
					end;
				!backup.(!nb_elements) <- bn;
				incr nb_elements
			done;
			!backup
		end
		

					
let r = "B", Cons(0, [])::Cons(1, [])::Cons(1, (Elem "B")::[])::Cons(1,(Elem "B")::(Elem "B")::[])::[];; 
count 10 r;;
