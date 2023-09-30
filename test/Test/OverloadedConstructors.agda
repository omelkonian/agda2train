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

record X : Set where
  constructor mk
  field x : Nat

x : X
x = mk n

record 𝕏 : Set where
  constructor mk
  field 𝕩 : ℕ

𝕩 : 𝕏
𝕩 = mk 𝕟
