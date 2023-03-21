module Abel where

-- open import Algebra.Bundles
-- open import Level using (0ℓ)
-- open import Agda.Builtin.Equality

-- variable A : Set

-- postulate _≈_ : A → A → Set

Op₁ : Set → Set
Op₁ A = A → A

Op₂ : Set → Set
Op₂ A = A → A → A

-- Commutative : Op₂ A → Set
-- Commutative _∙_ = ∀ x y → (x ∙ y) ≈ (y ∙ x)

-- record IsAbelianGroup (∙ : Op₂ A) : Set where
--   field comm : Commutative ∙

-- record AbelianGroup : Set₁ where
--   infixl 7 _∙_
--   field
--     Carrier        : Set
--     _∙_            : Op₂ Carrier
--     isAbelianGroup : IsAbelianGroup _∙_

--   open IsAbelianGroup isAbelianGroup public

-- record IsRing (+ : Op₂ A) : Set where
--   field +-isAbelianGroup : IsAbelianGroup +
--   open IsAbelianGroup +-isAbelianGroup public
--     renaming (comm to +-comm)

-- record Ring : Set₁ where
--   infixl 6 _+_
--   field Carrier : Set
--         _+_     : Op₂ Carrier
--         isRing  : IsRing _+_

--   open IsRing isRing public

--   +-abelianGroup : AbelianGroup
--   +-abelianGroup = record { isAbelianGroup = +-isAbelianGroup }

--   open AbelianGroup +-abelianGroup public

-- record IsCommutativeRing (+ : Op₂ A) : Set where
--   field isRing : IsRing +
--   open IsRing isRing public

-- record CommutativeRing : Set₁ where
--   infixl 6 _+_
--   field
--     Carrier           : Set
--     _+_               : Op₂ Carrier
--     isCommutativeRing : IsCommutativeRing _+_

--   open IsCommutativeRing isCommutativeRing public

--   ring : Ring
--   ring = record { isRing = isRing }

--   open Ring ring public using (+-abelianGroup)
-- postulate xor-∧-commutativeRing : CommutativeRing 0ℓ 0ℓ

-- data Bool : Set where
--   true false : Bool

-- postulate
--   not : Op₁ Bool
--   xor _∨_ _∧_ : Op₂ Bool
--   ∨-∧-booleanAlgebra : BooleanAlgebra 0ℓ 0ℓ
--   xor-is-ok : ∀ x y → x xor y ≡ (x ∨ y) ∧ not (x ∧ y)


--------------------------------------------------------------------------------------

open import Level using (0ℓ)
open import Algebra.Bundles
open import Function.Base using (id)

module XorRing
  {Carrier : Set}
  (_≈_ : Carrier → Carrier → Set)
  (⊥ : Carrier) (_xor_ : Op₂ Carrier) where

  open import Algebra.Definitions _≈_
  open import Algebra.Structures _≈_

  private
    infixl 6 _⊕_
    _⊕_ : Op₂ Carrier
    _⊕_ = _xor_
  postulate
    ⊕-comm : Commutative _⊕_
    ⊕-⊥-isGroup : IsGroup _⊕_ ⊥ id

  ⊕-⊥-isAbelianGroup : IsAbelianGroup _⊕_ ⊥ id
  ⊕-⊥-isAbelianGroup = record
    { isGroup = ⊕-⊥-isGroup
    ; comm    = ⊕-comm
    }

  ⊕-⊥-abelianGroup : AbelianGroup _ _
  ⊕-⊥-abelianGroup = record
    { isAbelianGroup = ⊕-⊥-isAbelianGroup }

data Bool : Set where
  true false : Bool
postulate not : Op₁ Bool; _xor_ _∨_ _∧_ : Op₂ Bool

postulate _≡_ : Bool → Bool → Set
-- open import Algebra.Definitions _≡_
-- open import Algebra.Structures _≡_

-- postulate
--   ∨-∧-isBooleanAlgebra : IsBooleanAlgebra _∨_ _∧_ not true false

-- ∨-∧-booleanAlgebra : BooleanAlgebra 0ℓ 0ℓ
-- ∨-∧-booleanAlgebra = record
--   { isBooleanAlgebra = ∨-∧-isBooleanAlgebra
--   }

xor-∧-commutativeRing : AbelianGroup 0ℓ 0ℓ
xor-∧-commutativeRing = ⊕-⊥-abelianGroup
  where open XorRing _≡_ false _xor_
