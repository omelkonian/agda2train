{-# OPTIONS -v tc.check.internal:50 -v tc.infer.internal:50 -v tc.conv.fun:50 #-}
module OpenModule where

module M {A : Set} (P : A → Set) where

  data _？ (x : A) : Set where
    mk : P x → x ？

  f : ∀ x → x ？ → x ？
  f _ p = p

postulate
  A : Set
  P : A → Set
open M P
