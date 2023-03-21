module FunNary2 where

open import Level using (Level; 0ℓ; _⊔_)

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

record _×_ {a b} (A : Set a) (B : Set b) : Set (a ⊔ b) where
  constructor _,_
  field fst : A
        snd : B
open _×_

data ⊤ {a} : Set a where
  instance tt : ⊤

--

Levels : ℕ → Set
Levels zero    = ⊤
Levels (suc n) = Level × Levels n

⨆ : ∀ n → Levels n → Level
⨆ zero    _        = Level.zero
⨆ (suc n) (l , ls) = l ⊔ (⨆ n ls)

Sets : ∀ n (ls : Levels n) → Set (Level.suc (⨆ n ls))
Sets zero    _        = ⊤
Sets (suc n) (l , ls) = Set l × Sets n ls

infixr -1 _<$>_

_<$>_ : (∀ {l} → Set l → Set l) → ∀ {n ls} → Sets n ls → Sets n ls
_<$>_ f {zero}   as        = tt
_<$>_ f {suc n}  (a , as)  = f a , (f <$> as)
