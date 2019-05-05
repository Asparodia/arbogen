open Printf
open Grammar

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
  index = ref 0 and
  i = ref 1 in 
  while !i < Array.length base && not !modification do
    if base.(!index) - base.(!i) == 0 then index := !i
    else if base.(!index) - base.(!i) >= 2 then
      begin
        base.(!index) <- base.(!index) - 1;
        base.(!i) <- base.(!i) + 1;
        modification := true
      end;
    incr i
  done;
  let duplicate = ref 1 and
  factor = ref 1 in
  for i=0 to (Array.length base -2) do
    if base.(i) == base.(i+1) then 
      begin
        incr duplicate;
        factor := !factor * !duplicate
      end
    else
      duplicate := 1
  done;
  !modification, factorial (Array.length base) / !factor, base
            
(* Gestion de fichiers *)      
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
      
let replace input output = 
  Str.global_replace (Str.regexp_string input) output
    
let file_of_rule (rname,comps) =
  replace "*" ""(replace "Elem" "El"(replace "Cons" "Cs"(replace "::" ""(replace ">" "-" (replace "<" "-"(replace ")" ""(replace "(" ""(replace ";" ".cnt" (replace " " "" (string_of_rule (rname,comps)))))))))))

(* Count *)  
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
  let filename = file_of_rule r in
  if not (Sys.file_exists filename) then 
    begin 
      let create = open_out filename in 
      close_out create
    end;
  let lines = read_file (file_of_rule r) in 
  let fileWriter = open_out_gen [Open_append] 744 (file_of_rule r) in 
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
        incr nb_elements;
        fprintf fileWriter "%s\n" (string_of_int bn)
      done;
      close_out fileWriter;
      !backup
    end
  
(* Tests *)  

(*          
let r = "B", Cons(0, [])::Cons(1, [])::Cons(1, (Elem "B")::[])::Cons(1,(Elem "B")::(Elem "B")::[])::[];; 
count 10 r;;
*)

(*
let b = "B", Cons(0, [])::Cons(1,(Elem "B")::(Elem "B")::[])::[];;
count 30 b;;
*)

(*
let b1 = "B", Cons(0, [])::Cons(1,[])::Cons(2,(Elem "B"::[]))::Cons(1,(Elem "B")::(Elem "B")::[])::[];;
count 30 b1;;
*)


let b2 = "B", Cons(0, [])::Cons(1,[])::Cons(1,(Elem "B")::(Elem "B")::[])::Cons(2,(Elem "B")::(Elem "B")::(Elem "B")::[])::Cons(3,(Elem "B")::(Elem "B")::(Elem "B")::(Elem "B")::[])::[];;
count 30 b2;;
















