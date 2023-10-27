module Test.Issue13 where

private variable A B C : Set

postulate
  _∘_ : (B → C) → (A → B) → (A → C)
  _×_ _⊢>_ : Set → Set → Set
  List Map : Set → Set
  K V : Set

module List where postulate
  map : (A → B) → List A → List B

module AVL where postulate
  fromList : List (K ⊢> V) → Map V
  fromPair : K × V → (K ⊢> V)

fromList : List (K × V) → Map V
fromList = AVL.fromList ∘ List.map AVL.fromPair
