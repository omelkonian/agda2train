module EqReasoning0 (_∼_ : Set → Set → Set) where

-- _≡⟨⟩_ : ∀ x {y} → x ∼ y → x ∼ y
-- _ ≡⟨⟩ x∼y = x∼y

data _IsRelatedTo_ (x y : Set) : Set where
  relTo : (x∼y : x ∼ y) → x IsRelatedTo y

_≡⟨⟩_ : ∀ x {y} → x IsRelatedTo y → x IsRelatedTo y
_ ≡⟨⟩ x∼y = x∼y
