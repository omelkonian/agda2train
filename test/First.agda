-- postulate A : Set
variable A : Set

id : A → A
id x = x

data Bool : Set where
  true false : Bool

not : Bool → Bool
not true  = false
not false = true

-- ite : {A : Set} → Bool → A → A → A
-- ite true x y = id x
-- ite false x y = id y

ite2 : {A : Set} → Bool → A → A → A
ite2 true = λ x y → x
ite2 false = λ x y → y

-- module _ {A : Set} where
--   ite3 : Bool → A → A → A
--   ite3 true  = λ x y → x
--   ite3 false = λ x y → y

-- module _ {A : Set} (b : Bool) where
--   ite4 : A → A → A
--   ite4 x y = go b
--     where
--       go : Bool → A
--       go true = x
--       go false = y

-- {-# NON_TERMINATING #-}
-- loop : Bool
-- loop = loop

-- test1 = ite false loop true

-- data Nat : Set where
--   zero : Nat
--   suc  : Nat → Nat

-- one = suc zero
-- two = suc one
-- three = suc two

-- pred : Nat → Nat
-- pred zero = zero
-- pred (suc n) = n


-- _+_ : Nat → Nat → Nat
-- zero + n = n
-- (suc m) + n = suc (m + n)

-- twice : Nat → Nat
-- twice zero = zero
-- twice (suc n) = suc (suc (twice n))

-- pow2 : Nat → Nat
-- pow2 zero = suc zero
-- pow2 (suc n) = twice (pow2 n)

-- consume : Nat → Nat
-- consume zero = zero
-- consume (suc n) = consume n

-- test2 = consume (pow2 (twice (twice (twice three))))


-- data Vec (@0 A : Set) : @0 Nat → Set where
--   nil : Vec A zero
--   con : {@0 n : Nat} → A → Vec A n → Vec A (suc n)

-- head : {@0 A : Set} {@0 n : Nat} → Vec A (suc n) → A
-- head (con x xs) = x

-- tail : {@0 A : Set} {@0 n : Nat} → Vec A (suc n) → Vec A n
-- tail (con x xs) = xs

-- map : {@0 A B : Set} {@0 n : Nat} → (A → B) → Vec A n → Vec B n
-- map f nil = nil
-- map f (con x xs) = con (f x) (map f xs)

-- test3 = head (tail (map suc (con zero (con (suc zero) (con (suc (suc zero)) nil)))))

-- -- Testing that names are properly sanitized
-- 123'#|H\x65llo = zero

-- test4 = 123'#|H\x65llo

-- module M (n : Nat) where
--   fie : Nat
--   fie = suc n

--   foe : Nat
--   foe = suc fie

-- open M (suc (suc zero))

-- fun : Nat
-- fun = fie + foe
