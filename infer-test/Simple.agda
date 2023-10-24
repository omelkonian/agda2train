module Simple where

open import SimpleLib

Theorem1 : Set
Theorem1 = A -> B

theorem : Theorem1
theorem = lemma1

theorem1 :  A -> B
theorem1 = lemma1

Theorem2 : Set
Theorem2 = B -> A

theorem2 :  B -> C
theorem2 = lemma2

theorem3 : A -> C
theorem3 x = lemma2 (lemma1 x)


