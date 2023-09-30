module Test.PrivateDefinitions where

open import Agda.Builtin.Nat using (Nat; suc; zero)

_ : Nat
_ = suc (suc zero)
-- the holes here are lost, since this is a private definition
