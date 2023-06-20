module ReasoningSetoid where

postulate Y : Set; y : Y

module M (y : Y) where
  data X : Set where
    c1 : X
    c2 : X → X

open M y public
