module Reduction where

open import Agda.Builtin.Equality

-- ** example from https://github.com/agda/agda/issues/850

data A# : Set where
  a# : A#

A : Set
A = A#

a : A
a = a#

f : A → A
f a# = a

eq : f a ≡ f a
eq = refl

-- expensive computation (should timeout) **

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

one = suc zero
two = suc one
three = suc two

twice : Nat → Nat
twice zero = zero
twice (suc n) = suc (suc (twice n))

pow2 : Nat → Nat
pow2 zero = suc zero
pow2 (suc n) = twice (pow2 n)

consume : Nat → Nat
consume zero = zero
consume (suc n) = consume n

test2 = consume (pow2 (twice (twice (twice three))))
