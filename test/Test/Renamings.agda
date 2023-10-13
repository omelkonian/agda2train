module Test.Renamings where

-- ** Renamings do not appear in the JSON terms, since the Agda compiler has
-- translated them away and resolved their true origin.
-- NB: they do however appear in the pretty-printed parts of the JSON.

open import Agda.Builtin.Nat using (zero) renaming (Nat to ℕ)

n : ℕ
n = zero
