module Test.Fields where

open import Agda.Builtin.String using (String)

record User : Set where
  field name : String
open User public using (name)

postulate koks : User

-- kokos = koks .name
kokos = name koks
