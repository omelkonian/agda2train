module Test.PropEq where

open import Agda.Builtin.Equality
open import Agda.Builtin.Nat using (Nat; suc; zero)

-- x : ℕ
-- x = -- λ x → x : Λ-TERM
--     -- 5       : Λ-TERM

-- ADT
-- data Bool : Set where
--   True False : Bool

-- not : Bool → Bool
-- not True  = False
-- not False = True

data Bool : Set where
  True False : Bool

reflℕ : (n : Nat) → n ≡ n
reflℕ n = refl {x = n}

symℕ : (n m : Nat) → n ≡ suc m → {!!}
symℕ (suc k) .k refl = {!!}

pred : Nat → Nat
pred zero    = zero
pred (suc n) = suc n

id : ∀ {ℓ} {A : Set ℓ} → A → A
id x = x

{- ** JSON **
{
  "name": "Test.PropEq.not<10>",
  "type": {
    "pretty": "Bool → Bool",
    "tag": "Pi",
    "name": "_",
    "domain": {
      "tag": "ScopeReference",
      "name": "Test.PropEq.Bool<4>"
    },
    "codomain": {
      "tag": "ScopeReference",
      "name": "Test.PropEq.Bool<4>"
    }
  },
  "definition": {
    "pretty": "\"⊜\" ([zero] ⊝ zero)
                     ({n : Nat}[suc n] ⊝ n)",
    "pretty": "\"⊜\" ([True] ⊝ False) ([False] ⊝ True)",
    "tag": "Function",
    "clauses": [
      {
        "telescope": [
          {ℓ : Level}
          {A : Set @0}
          (x : @0)
        ],
        "lhs"/"patterns": [
          (suc @2) ~ App suc @2
        ]
        "rhs"/"body": {
          "tag": "ScopeReference",
          "name": "Test.PropEq.Bool.False<8>"
        }
      },
      {
        "tag": "ScopeReference",
        "name": "Test.PropEq.Bool.True<6>"
      }
    ]
  }}

-}

-- not b = if b then False else True
-- not = λ where True  → False
--               False → True
-- not = λ { True  → False
--         ; False → True
--         }
-- ↝ $57201759 True  = False
-- ↝ $57201759 False = True
-- not b = case b of λ where
--   True  → False
--   False → True


-- sym : ∀ {A : Set} {x y : A} → x ≡ y → y ≡ x
-- -- sym refl = refl
-- sym = λ refl → refl
-- -- if b then t else f
-- -- if b then t else f == case b of {t -> t; f -> f}
-- -- λ p → case p of {refl → refl}


-- -- trans : ∀ {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
-- -- trans refl eq = eq
