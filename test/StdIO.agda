{-# OPTIONS --guardedness #-}

module StdIO where

open import Data.String.Base using (String; _≈_)

-- cmp : Relation.Binary.Rel String _
cmp : String → String → Set
cmp = _≈_
