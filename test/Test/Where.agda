module Test.Where where

open import Agda.Builtin.Nat using (Nat; suc)

f f² : Nat → Nat
f n = g (suc n)
  where postulate g : Nat → Nat
  -- c.f. `Test.Where._.g<12>` in the JSON

f² n = g (suc n)
  where postulate g : Nat → Nat
  -- c.f. `Test.Where._.g<22>` in the JSON
