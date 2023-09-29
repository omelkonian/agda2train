module Test.Sum where

data _⊎_ (A : Set) (B : Set) : Set where
  inj₁ : A → A ⊎ B
  inj₂ : B → A ⊎ B

_ : Set → Set → Set
_ = _⊎_

_ : ∀ {A B : Set} → A → A ⊎ B
_ = inj₁

_ : ∀ {A B : Set} → B → A ⊎ B
_ = inj₂
