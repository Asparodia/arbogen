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
  let n = ref (Z.succ m) in 
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
          if !n <= Z.of_int !step then raise Breakloop;
          n := Z.sub !n (Z.of_int !step);
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
  |[] -> (*[]*)Node("","",[])
  |hd::t ->
    (*let s = ref ["("] in *)
    let s = ref [] in
    if number_of_recursions hd == 0 && number_of_nodes hd == size && !index == Z.zero then 
      (*["("^(string_of_int (number_of_nodes hd))^")"]*)
      Leaf("","")
    else if number_of_recursions hd == 0 && number_of_nodes hd == size && backup.(size) > Z.zero then 
      (aux t l_original backup size (ref (Z.pred !index)))
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
          let product = ref Z.one in
          for i=0 to (Array.length !partition -1) do
            product := Z.mul !product backup.(!partition.(i));
          done;
          if (Z.mul (Z.of_int !factor) !product) >= (Z.succ !index) then
            begin
              let c = combination (Z.div !index !product) !partition in
              index := Z.erem !index !product;
             (* s := !s @ [string_of_int (number_of_nodes hd)];*)
              for i=0 to (Array.length c -1) do
                product := Z.div !product backup.(c.(i));
                (*s := !s @ (aux l_original l_original backup c.(i) (ref(Z.div !index !product)));*)
                s := !s @ [ref(aux l_original l_original backup c.(i) (ref(Z.div !index !product)))];
                index := Z.erem !index !product;
                done;
              isModified := false;
              fin_parenthese := true
              end
          else
            begin
              index := Z.sub !index (Z.mul (Z.of_int !factor) !product);
               let a,b,c = next_partition_with_factor !partition in
               isModified := a;
               factor := b;
               partition := c;
            end
        done;
        if !fin_parenthese then 
          (*!s @ [")"]*)
          Node("","",!s)
        else
          (aux t l_original backup size index)
      end
    else
      (aux t l_original backup size index)



(*
let unranking size index rule = 
  let backup = count size rule in
  String.concat "" (aux (get_comps_from_rule rule) (get_comps_from_rule rule) backup size (ref index))
*)

let unranking size index rule = 
  let backup = count size rule in
  aux (get_comps_from_rule rule) (get_comps_from_rule rule) backup size (ref index)


(* Test *)

(*
let r = "B", Cons(0, [])::Cons(1, [])::Cons(1, (Elem "B")::[])::Cons(1,(Elem "B")::(Elem "B")::[])::[];; 
let a_r = unranking 5 (Z.of_int 3) r;;
*) 

(*
let b = "B", Cons(0, [])::Cons(1,(Elem "B")::(Elem "B")::[])::[];;
let a_b = unranking 20 (Z.of_int 120) r;;
*)

(*
let b1 = "B", Cons(0, [])::Cons(1,[])::Cons(2,(Elem "B"::[]))::Cons(1,(Elem "B")::(Elem "B")::[])::[];;
let a_b1 = unranking 10 (Z.of_int 60) b1;;
*)

(*
let b2 = "B", Cons(0, [])::Cons(1,[])::Cons(1,(Elem "B")::(Elem "B")::[])::Cons(2,(Elem "B")::(Elem "B")::(Elem "B")::[])::Cons(3,(Elem "B")::(Elem "B")::(Elem "B")::(Elem "B")::[])::[];;
let a_b2 = unranking 4 (Z.of_int 0) b2;;

*)






























