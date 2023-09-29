module Test.PiVsFun where

postulate
  _≡_ : Set → Set → Set
  refl : ∀ {x y : Set} → x ≡ y -- λ λ _≡_ #1 #0

f : ∀ x → Set → x ≡ x -- λ ƛ _≡_ #0 ＃0
f x _ = refl {x}{x} -- λ λ refl {#1} {#1}

g : (x y : Set) → x ≡ y -- λ λ _≡_ #1 #0
g x y = refl {x}{y} -- λ λ refl {#1} {#0}
