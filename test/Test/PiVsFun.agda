module Test.PiVsFun where

postulate
  _≡_ : Set → Set → Set
  refl : ∀ {x y : Set} → x ≡ y -- λ λ _≡_ #1 #0

f : ∀ x → Set → x ≡ x -- λ ƛ _≡_ #0 ＃0
f x _ = refl {x}{x} -- λ λ refl {#1} {#1}

g : (x y : Set) → x ≡ y -- λ λ _≡_ #1 #0
g x y = refl {x}{y} -- λ λ refl {#1} {#0}


-----------------------------------------------------

-- id : {A : Set} → A → A
-- id x = x

-- id′ : {A : Set} → (x : A) → A
-- id′ x = x

-- id″ : ∀ {A : Set} (x : A) → A
-- id″ x = x

-----------------------------------------------------

-- const : ∀ {A B : Set} → A → B → A
-- const a b = a

-- const′ : ∀ {A B : Set} (a : A) (b : B) → A
-- const′ a b = a

-----------------------------------------------------

-- id : (A : Set) → Set
-- id A = A

-- id′ : Set → Set
-- id′ A = A

-- const : (A B : Set) → Set
-- const A B = A

-- const- : (A B : Set) → Set
-- const- A _ = A

-- const′ : (A : Set) → Set → Set
-- const′ A B = A

-- const′- : (A : Set) → Set → Set
-- const′- A _ = A

-- const″ : Set → Set → Set
-- const″ A B = A

-- const″- : Set → Set → Set
-- const″- A _ = A

-----------------------------------------------------
