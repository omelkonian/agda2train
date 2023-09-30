module Test.Clauses where

open import Data.Nat.Base using (ℕ; zero; suc)

f : ℕ → ℕ
f zero = zero
f (suc n) = suc (suc (f n))
