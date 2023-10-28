module Test.DependencyLevels where

open import Agda.Primitive using (Level)

postulate
  -- * depLvl = 0
  ℕ   : Set
  Fin : ℕ → Set
  _+_ : ℕ → ℕ → ℕ
  -- * depLvl = 2
  dep2 : (A : Set) → A → A
  lvl  : (a : Level) → (A : Set a) → A → A
  -- * depLvl = 3
  dep3 : (n : ℕ) → Fin n → Fin n → Fin n
  -- * depLvl = 4
  dep4 : (n : ℕ) → Fin n → Fin n → Fin (n + n)
  id   : (A B : Set) → (A → B) → A → B
