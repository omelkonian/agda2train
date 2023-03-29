import Data.String

module foo where

  data ℕ : Set where
    Z : ℕ
    S : ℕ → ℕ

  infixl 5 _+_
  _+_ : ℕ → ℕ → ℕ
  Z + y = y
  S x + y = S (x + y)

  cow : ℕ → ℕ
  cow = λ { Z → S Z ; (S x) → x }

  module _ {Real : Set} where

    record Complex : Set where
      constructor ⟨_,_⟩
      field
        re : Real
        im : Real

    rabbit : Complex → Real
    rabbit ⟨ x , y ⟩ = x

    open Complex

    hare : Complex → Real
    hare z = re z

  module Foo where
    module Bar where
      id : ∀ {ℓ} {A : Set ℓ} → A → A
      id x = x

  t = Foo.Bar.id "This string contains forty-two characters."
