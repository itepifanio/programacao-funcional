(* Todos os tipos de dados de Coq
 são definidos na própria linguagem *)

(* Define-se os tipos com Inductive,
 com identificador, tipo do tipo e sua
 definição em | termo : tipo *)

Inductive dia : Type :=
  | segunda : dia
  | terca   : dia
  | quarta  : dia
  | quinta  : dia
  | sexta   : dia
  | sabado  : dia
  | domingo : dia.

(* Para enunciar um teorema usa-se 
 a palavra reservada Theorema, seguido
 do identificador e proposição *)

(* As provas são enunciadas Por proof e
 finalizadas por Qued *)

(* reflexivity verifica se os termos da
 igualdade são os mesmos *)

Theorem sextaSexta : sexta = sexta.
Proof.
  reflexivity.
Qed.

(* Programas não recursivo são escritos
 como Definition, seguido pelo nome, 
 argumentos e tipo de retorno *)

Definition proximo_dia_semana (d:dia) : dia :=
  match d with
  | segunda => terca
  | terca   => quarta
  | quarta  => quinta
  | quinta  => sexta
  | sexta   => sabado
  | sabado  => domingo
  | domingo => segunda
  end.

(* Compute (proximo_dia_semana quinta). *)

(* exemplo de teste sobre uma função *)
Example teste_proximo_dia_semana:
  (proximo_dia_semana (proximo_dia_semana quarta)) = sexta.
Proof.
  simpl.
  reflexivity.
Qed.