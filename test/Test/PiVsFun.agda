module Test.PiVsFun where

postulate
  _≡_ : Set → Set → Set
  refl : ∀ {x y : Set} → x ≡ y -- λ λ _≡_ #1 #0

f : ∀ x → Set → x ≡ x -- λ ƛ _≡_ #0 ＃0
f x _ = refl {x}{x} -- λ λ refl {#1} {#1}

g : (x y : Set) → x ≡ y -- λ λ _≡_ #1 #0
g x y = refl {x}{y} -- λ λ refl {#1} {#0}

fi : ∀ {x} → {Set} → x ≡ x -- λ ƛ _≡_ #0 ＃0
fi {x} = refl {x}{x} -- λ λ refl {#1} {#1}

gi : {x y : Set} → x ≡ y -- λ λ _≡_ #1 #0
gi {x} {y} = refl {x}{y} -- λ λ refl {#1} {#0}

_ : ∀ x → Set → x ≡ x -- λ ƛ _≡_ #0 ＃0
_ = λ x _ → refl {x}{x} -- λ λ refl {#1} {#1}

_ : (x y : Set) → x ≡ y -- λ λ _≡_ #1 #0
_ = λ x y → refl {x}{y} -- λ λ refl {#1} {#0}

private variable x y : Set

id : x ≡ y → x ≡ x
id eq = refl
