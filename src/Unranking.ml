(*open Count*)

let rec factorial n = 
	if n <= 1 then 1 
	else n * (factorial (n-1))

let rec remove l indice = match l with
| [] -> []
| hd::t -> if indice == 0 then t else hd::(remove t (indice-1))

let frequencies p =
	let f = ref [[|p.(0); 1|]] in
	for i=1 to (Array.length p -1 ) do
		if p.(i) == ((List.hd !f).(0)) then 
			(List.hd !f).(1) <- (List.hd !f).(1) +1 
		else
			f := [|p.(i);1|] :: !f
	done;
	!f
		
		
exception Breakloop;;

let combination m p = 
	let n = ref (m+1) in
	let f = ref (frequencies p) in
	let c = Array.copy p in
	if (Array.length p) == 1 then p
	else
		begin
			for i=0 to (Array.length c -1) do
				let index = ref 0 in
				try
					while true do
						let step = ref 1 in
						for j=0 to (List.length !f -1) do
							if j == !index then
								begin
								step := !step * (factorial ((List.nth !f j).(1) -1));
								end
							else
								step := !step * (factorial (List.nth !f j).(1));
						done;
						step := (factorial (Array.length c - i -1)) / (!step);
						if !n <= !step then raise Breakloop;
						n := !n - !step;
						incr index
					done;
				with Breakloop -> 
					begin
						c.(i) <- (List.nth !f !index).(0);
						(List.nth !f !index).(1) <- (List.nth !f !index).(1) -1;
						if (List.nth !f !index).(1) == 0 then f := (remove !f !index);
					end;
			done;
			c;
		end 
		

(*module Repertory = Map.Make(String);;
				
let unranking size index rule = 
	let backup = count(size, rule) in
	let r = Repertory.empty in 
		let rec aux size index = 
			let s = ref ["("] in
			let key = (string_of_int size) ^ "/" ^ (string_of_int index) in
			if Repertory.mem key r then 
				Repertory.find key r 
			else
				begin
					
				end 

	
*)	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	