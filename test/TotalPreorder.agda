postulate
  A : Set
  a0 : A

record X : Set where
  field a : A

record Y (a : A) : Set where
  field x : X
  open X x public

record Z : Set where
  field y : Y a0
  open Y y public

  _ : A
  _ = a
