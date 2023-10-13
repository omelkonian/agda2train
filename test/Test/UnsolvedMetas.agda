{-# OPTIONS --allow-unsolved-metas #-}
module Test.UnsolvedMetas where

open import Agda.Builtin.Nat using (Nat)

n : Nat
n = {!!}
