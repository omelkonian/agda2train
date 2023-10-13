module Test.Where where

open import Agda.Builtin.Nat using (Nat; zero; suc)

f : Nat → Nat
f n = g (suc n)
  where postulate g : Nat → Nat
  -- ** since `Test.Where.g` is a private definition,
  -- the JSON contains no information whatsoever about it
