module Test.ParametrisedDatatypes where

open import Agda.Builtin.String using (String)
open import Agda.Builtin.Nat using () renaming (Nat to ℕ)

infixr 2 _×_
postulate _×_ : Set → Set → Set
private variable A B : Set

-- ** an expression language, parametric over the type of variables and constants.
module _ (𝕀 : Set) {- module parameter -} where
  data Expr (𝕂 : Set) {- datatype parameter -} : Set → Set₁ where
    var : 𝕀 → Expr 𝕂 A
    con : ℕ → Expr 𝕂 ℕ
    _⊗_ : Expr 𝕂 A → Expr 𝕂 B → Expr 𝕂 (A × B)
  infixr 2 _⊗_

ex : Expr String ℕ (ℕ × (ℕ → ℕ) × ℕ)
ex = con 0 ⊗ var "increment" ⊗ con 1
