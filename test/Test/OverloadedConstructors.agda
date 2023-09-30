module Test.OverloadedConstructors where

data Nat : Set where
  O : Nat
  S_ : Nat → Nat

n : Nat
n = S S O

data ℕ : Set where
  O : ℕ
  S_ : ℕ → ℕ

𝕟 : ℕ
𝕟 = S S O
