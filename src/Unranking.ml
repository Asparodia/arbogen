open Count
open Grammar

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
  let n = ref (m +1) in 
  let f = ref (frequencies p) in
  let c = Array.copy p in
  if (Array.length p) == 1 then p 
  else
    begin
      let s = ref 1 in
      for j=0 to (List.length !f -1) do
        s := !s * (factorial (List.nth !f j).(1))
      done;
      let grand_n = ref (factorial (Array.length c -1)) in
      for i=0 to (Array.length c -1) do
        let index = ref 0 in
        try
        while true do
          let step = ref ((!grand_n * (List.nth !f !index).(1)) / !s) in
          if !n <= !step then raise Breakloop;
          n := !n - !step;
          incr index
        done;
        with Breakloop ->
          begin
            s := !s / (List.nth !f  !index).(1);
            grand_n := max 1 (!grand_n / (Array.length c -1));
            c.(i) <- (List.nth !f !index).(0);
            (List.nth !f !index).(1) <- (List.nth !f !index).(1) -1;
            if (List.nth !f !index).(1) == 0 then f := (remove !f !index);
          end
      done;
      c
    end

(* Unranking *)

let rec aux (l:component list) (l_original:component list) backup size index = match l with
  |[] -> []
  |hd::t ->
    let s = ref ["("] in 
    if number_of_recursions hd == 0 && number_of_nodes hd == size && !index == 0 then 
      ["("^(string_of_int (number_of_nodes hd))^")"]
    else if number_of_recursions hd == 0 && number_of_nodes hd == size && backup.(size) > 0 then 
      (aux t l_original backup size (ref (!index-1)))
    else if (number_of_recursions hd) != 0 && (number_of_nodes hd) <= size then
      begin
        let partition = ref (Array.make ((number_of_recursions hd)) 0) in
        !partition.(0) <- size - number_of_nodes hd;
        let isModified = ref true in
        let f = frequencies !partition in 
        let factor = ref 1 in
        let rec produit paire = match paire with
          |[] -> 1
          |hd::t -> (factorial hd.(1)) * produit t
          in factor := (factorial (Array.length !partition)) / (produit f);
        let fin_parenthese = ref false in
        while !isModified do
          let product = ref 1 in
          for i=0 to (Array.length !partition -1) do
            product := !product * backup.(!partition.(i));
          done;
          if !factor * !product -1 >= !index then
            begin
              let c = combination (!index / !product) !partition in
              index := !index mod !product;
              s := !s @ [string_of_int (number_of_nodes hd)];
              for i=0 to (Array.length c -1) do
                product := !product / backup.(c.(i));
                s := !s @ (aux l_original l_original backup c.(i) (ref(!index / !product)));
                index := !index mod !product;
                done;
              isModified := false;
              fin_parenthese := true
              end
          else
            begin
              index := !index - !factor * !product;
               let a,b,c = next_partition_with_factor !partition in
               isModified := a;
               factor := b;
               partition := c;
            end
        done;
        if !fin_parenthese then 
          !s @ [")"]
        else
          (aux t l_original backup size index)
      end
    else
      (aux t l_original backup size index)




let unranking size index rule = 
  let backup = count size rule in
  String.concat "" (aux (get_comps_from_rule rule) (get_comps_from_rule rule) backup size (ref index))

(* Test *)

(*
let r = "B", Cons(0, [])::Cons(1, [])::Cons(1, (Elem "B")::[])::Cons(1,(Elem "B")::(Elem "B")::[])::[];; 
print_endline (unranking 5 3 r);;
*) 

(*
let b = "B", Cons(0, [])::Cons(1,(Elem "B")::(Elem "B")::[])::[];;
print_endline (unranking 20 120 r);;
*)

(*
let b1 = "B", Cons(0, [])::Cons(1,[])::Cons(2,(Elem "B"::[]))::Cons(1,(Elem "B")::(Elem "B")::[])::[];;
print_endline (unranking 10 60 b1);;
*)

let b2 = "B", Cons(0, [])::Cons(1,[])::Cons(1,(Elem "B")::(Elem "B")::[])::Cons(2,(Elem "B")::(Elem "B")::(Elem "B")::[])::Cons(3,(Elem "B")::(Elem "B")::(Elem "B")::(Elem "B")::[])::[];;
print_endline (unranking 4 0 b2);;

for i=0 to 99 do print_endline(unranking 10 i b2) done;;
































