module With where

postulate P : Set

record Manifest-Σ (A : Set) (_ : A → P) : Set where
  constructor _,
  field proj₁ : A

mutual
  record Sig : Set₁ where
    eta-equality
    inductive
    constructor _≔_
    field s : Sig
          f : Record s → P

  record Record (s : Sig) : Set where
    eta-equality
    inductive
    constructor rec
    field fun : Manifest-Σ (Record (s .Sig.s)) (s .Sig.f)

mutual
  $_≔_ : ∀ (s : Sig) → (Record (s .Sig.s) → P) → Sig
  $_≔_ (s ≔ _) f = s ≔ f

  FAIL : ∀ {s : Sig} {f : Record (s .Sig.s) → P}
    → Record ($ s ≔ f)
    → Record s
  FAIL (rec r) with r
  ... | r′ = rec (Manifest-Σ.proj₁ r′ ,)
