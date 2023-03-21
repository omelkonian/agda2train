module WithOld where

postulate P : Set

record Manifest-Σ (A : Set) (_ : A → P) : Set where
  constructor _,
  field proj₁ : A

mutual
  data Signature : Set₁ where
    _≔_ : (Sig : Signature)
        → (Record Sig → P)
        → Signature

  ↓_ : Signature → Signature
  ↓_ (Sig ≔ _) = Sig

  record Record (Sig : Signature) : Set where
    eta-equality
    inductive
    constructor rec
    field fun : Record-fun Sig

  Record-fun : Signature → Set
  Record-fun (Sig ≔ f) = Manifest-Σ (Record Sig) f

mutual
  $_≔_ : ∀ (Sig : Signature) → (Record (↓ Sig) → P) → Signature
  $_≔_ (Sig ≔ _) a = Sig ≔ a

  ↓ : ∀ {Sig : Signature} {a : Record (↓ Sig) → P}
    → Record ($ Sig ≔ a)
    → Record Sig
  ↓ {Sig = _ ≔ _} (rec r) with r
  ... | r = rec (Manifest-Σ.proj₁ r ,)
