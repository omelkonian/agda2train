module EqReasoning where

module EqReasoning0 (A : Set) where
  postulate P : A → Set
  id : ∀ x → P x → P x
  id _ p = p

postulate B : Set
open {-import-} EqReasoning0 B public

postulate C : Set
open EqReasoning0 C public
  renaming (P to P′; id to id′)
