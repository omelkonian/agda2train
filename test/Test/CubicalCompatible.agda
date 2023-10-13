{-# OPTIONS --cubical-compatible #-}
module Test.CubicalCompatible where

open import Agda.Builtin.Equality

trans trans′ : ∀ {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans  refl eq   = eq
trans′ refl refl = refl
