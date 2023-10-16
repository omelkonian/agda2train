module Test.PrivateDefinitions where

open import Agda.Builtin.Nat using (Nat; suc; zero)

-- ** The holes here are lost, since this is a private definition,
-- i.e. in the JSON we see `"scope-local": []`.
_ : Nat
_ = suc (suc zero)


-- ** A more serious issue when the terms refer to some private definition,
-- e.g. a field of a record that we are not `open`ining to the `public`.
record X : Set where
  field x : Nat
open X

proj : X → Nat
proj = x

-- ** non-record example
private
  f : Nat → Nat → Nat
  f = Agda.Builtin.Nat._+_

g : Nat → Nat
g x = f x (f x 0)
