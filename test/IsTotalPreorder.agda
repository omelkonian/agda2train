postulate A : Set; _≈_ : A → A → Set

Reflexive : (A → A → Set) → Set
Reflexive _∼_ = ∀ {x} → x ∼ x

record IsEquivalence : Set where
  field refl  : Reflexive _≈_

record IsPreorder (_∼_ : A → A → Set) : Set where
  field isEquivalence : IsEquivalence
        reflexive     : ∀ {x y} → x ≈ y → x ∼ y

  module Eq = IsEquivalence isEquivalence

  refl : Reflexive _∼_
  refl = reflexive Eq.refl

record IsTotalPreorder (_≲_ : A → A → Set) : Set where
  field isPreorder : IsPreorder _≲_
  open IsPreorder isPreorder public

  _ : IsEquivalence
  _ = isEquivalence

  _ : ∀ {x y} → x ≈ y → x ≲ y
  _ = reflexive

  _ : Reflexive _≲_
  _ = refl

  _ : Reflexive _≈_
  _ = Eq.refl
