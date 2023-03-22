module OpenModule where

open import Importee

module M (A : Set) where
  postulate P : A → Set

  f : ∀ x → P x → P x
  f _ p = p

postulate A : Set
open M A public
