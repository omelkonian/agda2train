module Test.ContextShadowing where

postulate ℕ : Set; _+_ : ℕ → ℕ → ℕ; _≡_ : ℕ → ℕ → Set

double : (n : ℕ) (m n : ℕ) → m ≡ n → ℕ
double _ m n _ = n + m
