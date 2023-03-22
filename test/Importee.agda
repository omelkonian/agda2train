





postulate
  ℕ : Set
  𝟘 𝟙 𝟚 𝟛 : ℕ
  _≡_ : ℕ → ℕ → Set
  _+_ _*_ _/_ _-_ : ℕ → ℕ → ℕ
  +-comm : ∀ {x y} → (x + y) ≡ (y + x)
  +-assoc : ∀ {x y z} → (x + (y + z)) ≡ ((x + y) + z)
  example : (𝟙 + 𝟙) ≡ 𝟚

private postulate
  nastyℕ : ℕ
  nastyLem : (𝟙 + 𝟙) ≡ 𝟛

_+nasty : ℕ → ℕ
x +nasty with 𝟘
... | _ = x + nastyℕ

_+nasty' : ℕ → ℕ
_+nasty' = λ where x → x + 𝟘

_+2 : ℕ → ℕ
x +2 = x +n
  where
    n = 𝟚

    _+n : ℕ → ℕ
    _+n = _+ n
