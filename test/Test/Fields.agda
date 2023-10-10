module Test.Fields where

open import Agda.Builtin.String using (String)

record Name : Set where
  field first : String
        last  : String
open Name public

record User : Set where
  field name : Name
open User public

postulate koks : User

kokos = koks .name .first
