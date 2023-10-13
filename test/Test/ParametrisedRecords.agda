module Test.ParametrisedRecords where

-- ** extended version of Test.Fields where records are also parametrised.

record Name (NameId : Set) : Set where
  field first : NameId
        last  : NameId
open Name public

open import Agda.Builtin.String using (String)

record User : Set where
  field name : Name String
open User public

postulate koks : User

kokos = koks .name .first
