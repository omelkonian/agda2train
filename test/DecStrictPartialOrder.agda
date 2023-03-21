postulate
  A : Set
  a0 : A

record X : Set where
  field a : A

record Y (a : A) : Set where
  field x : X
  module XX = X x

record Z : Set where
  field y : Y a0
  module M = Y y
  -- open M public
