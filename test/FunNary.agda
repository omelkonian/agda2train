module FunNary where

open import Level using (Level; _⊔_)
open import Data.Nat.Base using (ℕ; zero; suc)
open import Data.Product using (_×_; _,_)
open import Data.Unit.Polymorphic.Base using (⊤)

private
  variable
    a b c : Level
    A : Set a
    B : Set b
    C : Set c

Levels : ℕ → Set
Levels zero    = ⊤
Levels (suc n) = Level × Levels n

⨆ : ∀ n → Levels n → Level
⨆ zero    _        = Level.zero
⨆ (suc n) (l , ls) = l ⊔ (⨆ n ls)

Sets : ∀ n (ls : Levels n) → Set (Level.suc (⨆ n ls))
Sets zero    _        = ⊤
Sets (suc n) (l , ls) = Set l × Sets n ls

Arrows : ∀ n {r ls} → Sets n ls → Set r → Set (r ⊔ (⨆ n ls))
Arrows zero    _        b = b
Arrows (suc n) (a , as) b = a → Arrows n as b

infixr 0 _⇉_
_⇉_ : ∀ {n ls r} → Sets n ls → Set r → Set (r ⊔ (⨆ n ls))
_⇉_ = Arrows _

infixr -1 _<$>_

_<$>_ : (∀ {l} → Set l → Set l) →
        ∀ {n ls} → Sets n ls → Sets n ls
_<$>_ f {zero}   as        = _
_<$>_ f {suc n}  (a , as)  = f a , (f <$> as)
