module Test.PropEq where

open import Agda.Builtin.Equality
open import Agda.Builtin.Nat using (Nat; suc)

reflℕ : (n : Nat) → n ≡ n
reflℕ n = refl {x = n}

symℕ : (n m : Nat) → n ≡ suc m → Nat
symℕ (suc k) .k refl = k
-- ** note the absurd clause for the `zero` case
-- ** also note the forced-ness information (`.k`) is lost
