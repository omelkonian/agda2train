module Test.Product where

open import Agda.Primitive using (_⊔_)

record Σ {a b} (A : Set a) (B : A → Set b) : Set (a ⊔ b) where
  constructor _,_
  field
    fst : A
    snd : B fst
open Σ public
