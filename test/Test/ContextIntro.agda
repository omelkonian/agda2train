module Test.ContextIntro where

open import Relation.Binary.PropositionalEquality
open import Data.Nat

{-
test : x → y → z → ⋯ → GOAL
test = λ x y z ⋯ → hole (: ~GOAL~)

lemma : x → y → z → ⋯ → GOAL
lemma = ?

test : x → y → z → ⋯ → GOAL
test = ?

test : (x : ℕ) → (y : ℕ) → x ≡ y → y ≡ x
-- test = {!!} -- : (x : ℕ) → (y : ℕ) → x ≡ y → y ≡ x
-- test = {!!} -- : (x : ℕ) → (y : ℕ = x) → _ → x ≡ x
-}

ctxIntro : (x : ℕ) → (y : ℕ) → x ≡ y → y ≡ x
ctxIntro x y refl = refl

postulate
  A B C : Set
  f : A → B
  _∘_ : (A → B) → (A → A) → (A → B)

-- go : A → B
-- go a = f ((λ x → x) a)

go : A → B
go = f ∘ {!!}

-- NB:

-- subholes : ℕ → ℕ → ℕ
-- -- subholes = {!!} -- λ x y → x + y
-- subholes x = _+_ x y
{-
Goal : (x : ℕ) → ℕ → ℕ
-}
-- subholes x y = {!!} -- x + y

-- subholes' : ℕ → ℕ → ℕ
-- subholes' x y = x + ?

-- _ : ℕ → ℕ → ℕ
-- _ = ?

-- _ : ℕ → ℕ → ℕ
-- _ = λ x → ?

-- _ : ℕ → ℕ → ℕ
-- _ = λ x y → ?

-- _ : ℕ → ℕ → ℕ
-- _ = λ x y → ? + ?
