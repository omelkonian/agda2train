-- -- open import Relation.Binary.Core using (Rel)
-- -- module Eq2 {a ℓ} {A : Set a} (_≈_ : Rel A ℓ)  where
-- module Eq2 where

-- -- open import Relation.Binary.Bundles using (Setoid)
-- open import MkSetoid

-- -- record Setoid : Set₁ where
-- --   infix 4 _≈_
-- --   field Carrier : Set
-- --         _≈_     : Carrier → Carrier → Set
-- -- postulate S : Setoid
-- open Setoid S

-- -- infix  4 _IsRelatedTo_
-- -- data _IsRelatedTo_ (x y : Carrier) : Set s₂ where
-- --   relTo : (x∼y : x ≈ y) → x IsRelatedTo y

-- -- _≡⟨⟩_ : ∀ x {y} → x IsRelatedTo y → x IsRelatedTo y
-- -- _ ≡⟨⟩ x∼y = x∼y

-- open import Eq _≈_ public
