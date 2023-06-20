{-# OPTIONS --cubical-compatible -WnoUnsupportedIndexedMatch #-}
open import Level
open import Agda.Builtin.Bool
open import Data.Empty using (⊥)
open import Data.Empty.Irrelevant using (⊥-elim)

private variable ℓ : Level

¬_ : Set ℓ → Set ℓ
¬ P = P → ⊥

data Reflects (P : Set ℓ) : Bool → Set ℓ where
  ofʸ : ( p :   P) → Reflects P true
  ofⁿ : (¬p : ¬ P) → Reflects P false

record Dec (P : Set ℓ) : Set ℓ where
  constructor _because_
  field does  : Bool
        proof : Reflects P does
open Dec public

-- pattern yes p =  true because ofʸ  p
-- pattern no ¬p = false because ofⁿ ¬p

-- data Dec {a} (A : Set a) : Set a where
--   yes : A  → Dec A
--   no  : ¬ A → Dec A

recompute : ∀ {a} {A : Set a} → Dec A → A → A
recompute (true because ofʸ x) _ = x
recompute _ x = x

-- recompute (false because ofⁿ ¬p) x = ⊥-elim (¬p x)
-- recompute (yes x) _ = x
-- recompute (no ¬p) x = ⊥-elim (¬p x)
