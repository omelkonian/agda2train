module SimpleLib where

data A : Set where
  aa : A
-- in the nominal approach (which is true to Agda), it is as if we had:

-- A : Set
-- A = <name "A">

-- aa : A
-- aa = <name "aa">

data B : Set where
  bb : B

data C : Set where
  cc  : C

lemma1 : A -> B
lemma1 aa = bb

lemma2 : B -> C
lemma2 bb = cc


