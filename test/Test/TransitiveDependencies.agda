module Test.TransitiveDependencies where

open import Agda.Builtin.String using (String; primShowNat)

-- ** the type of `showNat` is `Nat -> String`, but `Nat` is not in scope!
-- At the same time, it would be wrong (and ineffecient) to include such transitive
-- dependencies in this module's JSON; anyway the `Agda.Builtin.Nat` JSON
-- will necessarily be built so one could retrieve information from there directly.
showNat = primShowNat

-- NB: these scenarios give rise to *qualified* names in the pretty-printed JSON parts.
