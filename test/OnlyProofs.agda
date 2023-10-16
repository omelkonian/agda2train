module OnlyProofs where

open import Agda.Builtin.Nat using (Nat; suc)
open import Agda.Builtin.List using (List; [])
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Unit using (⊤; tt)

-- ** example of a non-dependent type signature that should not be considered a proof
suc² : Nat → Nat
suc² n = suc (suc n)

-- ** example of a dependent type signature that is morally not a proof
private variable n : Nat

data Vec (A : Set) : Nat → Set where
  []  : Vec A 0
  _∷_ : A → Vec A n → Vec A (suc n)

-- Vec ⊤ n ≈ n
-- ⇒ vsuc² ≈ suc²
-- ...but `vsuc²` is technically dependently-typed
vsuc² : Vec ⊤ n → Vec ⊤ (suc² n)
vsuc² xs = tt ∷ (tt ∷ xs)

-- ** example of technically dependent type signature that is not really a proof
id : ∀ {A : Set} → A → A
id x = x

∅ : ∀ {A : Set} → List A
∅ = []

-- ** example of `Set` quantification that is actually a proof
≡-sym : ∀ {A B : Set} → A ≡ B → B ≡ A
≡-sym refl = refl

-- ** example of technically non-dependent type signature that is actually a proof

record ℙ : Set where
  field suc-injective : ∀ {n m} → suc n ≡ suc m → n ≡ m
record ℙ² : Set where
  field suc²-injective : ∀ {n m} → suc² n ≡ suc² m → n ≡ m

nonDepProof : ℙ → ℙ²
nonDepProof 𝕡 =
  let open ℙ 𝕡 in
  record {suc²-injective = λ eq → suc-injective (suc-injective eq)}
