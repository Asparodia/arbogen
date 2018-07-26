%{
open Parsing
open Lexing
open Hashtbl
open Ast

let add_option a b =
  match a,b with
  | Some n, Some n' -> Some (n+n')
  | a, None -> a
  | None, b -> b

let rec fold_int f acc n =
  if n = 0 then f 0 acc
  else fold_int f (f n acc) (n-1)

let seq_leq n elem =
  match n with
  | 0 -> failwith "0 size sequence does not allowed"
  | n -> (fold_int
            (fun n acc ->
              if n = 0
              then
                (Some 0, []) :: acc
              else
                let prod_list =
                  fold_int
                    (fun n acc ->
                      if n = 0 then acc
                      else (Some (Ast.Elem elem)) :: acc)
                    [] n
                in (Some 0, prod_list) :: acc)
            [] n)

let seq_eq n elem =
  match n with
  | 0 -> failwith "0 size sequence does not allowed"
  | n -> (Some 0, (fold_int
                     (fun n acc ->
                       if n = 0 then acc
                       else (Some (Ast.Elem elem)) :: acc)
                     [] n))

let seq_geq n elem =
  (Some 0, (fold_int
              (fun n acc ->
                if n = 0 then acc
                else (Some (Ast.Elem elem)) :: acc)
              [(Some (Ast.Seq elem))] n))



%}


%token <int> NUMI
%token <float> NUMF
%token <string> IDENT

%token SEQ
%token EPSILON 
%token Z
%token UNION
%token PROD
%token CARD
%token LPAR
%token RPAR
%token COMMA
%token LEQ
%token GEQ
%token EQ

%token SET

%token EOF

%start start
%type <Ast.ast> start

%%

start:
 | options statement_list EOF { $1,$2 }
 | statement_list EOF { [],$1 }
 | EOF { raise End_of_file }


options:
 | option options { $1 :: $2 }
 | option { [$1] }


option:
 | SET IDENT NUMF { Param ($2, Vfloat $3) }
 | SET IDENT NUMI { Param ($2, Vint $3) }
 | SET IDENT IDENT { Param ($2, Vstring $3) }

statement_list:
  | statement COMMA statement_list {$1 :: $3}
  | statement {[$1]}

statement:
  | IDENT EQ expression {($1, $3)}

simple_expression:
  | EPSILON { ((Some 0), [])  }
  | Z { ((Some 1), []) }
  | IDENT { (None, [Some (Ast.Elem $1)])  }
  | SEQ LPAR IDENT RPAR { (Some 0, [Some (Ast.Seq $3)]) }
  | SEQ LPAR IDENT COMMA CARD EQ NUMI RPAR { seq_eq $7 $3 }
  | SEQ LPAR IDENT COMMA NUMI EQ CARD RPAR { seq_eq $5 $3 }
  | SEQ LPAR IDENT COMMA CARD GEQ NUMI RPAR { seq_geq $7 $3 }
  | SEQ LPAR IDENT COMMA NUMI LEQ CARD RPAR { seq_geq $5 $3 }

constrained_seq:
  | SEQ LPAR IDENT COMMA CARD LEQ NUMI RPAR { seq_leq $7 $3 }
  | SEQ LPAR IDENT COMMA NUMI GEQ CARD RPAR { seq_leq $5 $3 }

prod:
  | PROD LPAR component RPAR { $3 }

component:
 | simple_expression COMMA component
     {
       let w = add_option (fst $1) (fst $3) in
       match (snd $1) with
       | [] -> (w, snd $3)
       | e -> (w, e @ (snd $3))
     }
 | simple_expression { $1 }


simple_expression_list:
  | constrained_seq COMMA simple_expression_list { $1 @ $3 }
  | constrained_seq { $1 }
  | simple_expression COMMA simple_expression_list { $1 :: $3 }
  | prod COMMA simple_expression_list { $1 :: $3 }
  | prod { [$1] }
  | simple_expression { [$1] }

expression:
  | UNION LPAR simple_expression_list RPAR { $3 }
  | constrained_seq { $1 }
  | simple_expression { [$1] }
  | prod { [$1] }
