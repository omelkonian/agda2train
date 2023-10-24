module SimpleEnumLib where

data A : Set where
  aa : A

data B : Set where
  bb bb1 : B

data C : Set where
  cc cc2 cc3 : C

lemma1 : A -> B
lemma1 aa = bb

lemma2 : B -> C
lemma2 bb = cc
lemma2 bb1 = cc2


