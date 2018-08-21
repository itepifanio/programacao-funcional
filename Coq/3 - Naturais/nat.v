(* Criando módulo para os nat 
 abre e fecha o módulo *)
Module Nat.

(* Módulos são criados para fins 
 organizacionais *)
  Inductive nat : Type :=
    | O : nat
    | S : nat -> nat.

(* Note que nosso S é uma função *)

(* Função para recuperar o anterior 
 de um nat *)
Definition pred (n : nat) : nat :=
  match n with
    | O => O
    | S n' => n'
  end.

End Nat.

Check (S (S (S (S O)))).

Compute (pred 4).

Check S.

Check pred.