module Test.NominalTyping where

open import Agda.Builtin.Nat public using (Nat; zero)

data Nat′ : Set where
  zero : Nat′
  suc  : Nat′ → Nat′

ℕ : Set
ℕ = Nat

id : Nat → ℕ
id n = zero
