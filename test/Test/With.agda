module Test.With where

open import Agda.Builtin.Nat using (Nat; zero; suc)

postulate g : Nat → Nat

f : Nat → Nat
f n with g n
... | zero  = zero
... | suc n = suc (suc (g n))
-- c.f. `Test.With.with-10<12>` in the JSON
-- ** the with statement above internally generates a helper function `with-10<12>`
-- that performs the pattern matching and the body of `f` is substituted with a
-- "with-application" calling said function with the arguments `n` and `g n`.
