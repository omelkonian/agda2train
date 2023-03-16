module MkSetoid where

record Setoid : Set₁ where
  infix 4 _≈_
  field Carrier : Set
        _≈_     : Carrier → Carrier → Set
