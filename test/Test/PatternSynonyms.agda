module Test.PatternSynonyms where

open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Sigma using (Σ; _,_)

-- ** pattern synonyms have no effect in the JSON output,
-- since they have been translated away in the Internal syntax of Agda.
-- NB: we do however see them in the pretty-printed parts of the JSON.

pattern ⟨_,_⟩ x y = _,_ x y

dupNum : Nat → Σ Nat (λ _ → Nat)
dupNum n = ⟨ n , n ⟩
