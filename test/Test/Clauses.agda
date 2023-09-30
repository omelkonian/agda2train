module Test.Clauses where

open import Agda.Builtin.Nat using (Nat; zero; suc)

f : Nat → Nat
f zero = zero
f (suc n) = suc (suc (f n))
