module Test.With where

open import Agda.Builtin.Nat using (Nat; zero; suc)

postulate g : Nat → Nat

f : Nat → Nat
f n with g n
... | zero  = zero
... | suc n = suc (suc (g n))
