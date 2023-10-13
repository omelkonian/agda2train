module Test.OverloadedConstructors where

-- ** Data constructors can be overloaded, but we can always disambiguate
-- them by looking at the fully qualifed name that includes the datatype.
data Nat : Set where
  O : Nat
  S_ : Nat → Nat

n : Nat
n = S S O

data ℕ : Set where
  O : ℕ
  S_ : ℕ → ℕ

𝕟 : ℕ
𝕟 = S S O

-- ** Record constructors can be overloaded, but we cannot disambiguate
-- them like above; thus the need to include the unique name identifiers in the name,
-- e.g. Test.OverloadedConstructors.mk<26>.
record X : Set where
  constructor mk
  field x : Nat

ex-x : X
ex-x = mk n

record 𝕏 : Set where
  constructor mk
  field 𝕩 : ℕ

ex-𝕩 : 𝕏
ex-𝕩 = mk 𝕟
