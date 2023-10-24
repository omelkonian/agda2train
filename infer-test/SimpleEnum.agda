module SimpleEnum where

open import SimpleEnumLib

theorem : A -> B
theorem = lemma1

theorem2 :  B -> C
theorem2 = lemma2

theorem3 : A -> C
theorem3 x = lemma2 (lemma1 x)


