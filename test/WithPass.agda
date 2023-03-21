module WithPass where

postulate P : Set

record X (A : Set) : Set where
  constructor mk
  field unmk : A

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
    field fun : X (Record (s .Sig.s))

mutual
  $_≔_ : ∀ (s : Sig) → (Record (s .Sig.s) → P) → Sig
  $_≔_ (s ≔ _) f = s ≔ f

  FAIL : ∀ {s : Sig} {f : Record (s .Sig.s) → P}
    → Record ($ s ≔ f)
    → Record s
  FAIL (rec r) with r
  ... | r′ = rec (mk (r′ .X.unmk))
