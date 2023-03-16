module Eq {A : Set} (_∼_ : A → A → Set) where

-- _≡⟨⟩_ : ∀ x {y} → x ∼ y → x ∼ y
-- _ ≡⟨⟩ x∼y = x∼y

infix  4 _IsRelatedTo_
data _IsRelatedTo_ (x y : A) : Set where
  relTo : (x∼y : x ∼ y) → x IsRelatedTo y

_≡⟨⟩_ : ∀ x {y} → x IsRelatedTo y → x IsRelatedTo y
_ ≡⟨⟩ x∼y = x∼y
