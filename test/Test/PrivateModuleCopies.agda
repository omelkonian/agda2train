module Test.PrivateModuleCopies where

open import Agda.Builtin.Nat using (Nat; _+_)

module M (n : Nat) where
  y = n + n

open M 5
x = y
