module Bool where

import Prelude
    hiding (not, rem)
import Types ( Nat(..) )
import Nat

ifThenElse :: Bool -> Nat -> Nat -> Nat
ifThenElse True m n = m
ifThenElse False m n = n

not :: Bool -> Bool
not True = False
not False = True

leq :: Nat -> Nat -> Bool
leq O _ = True
leq _ O = False
leq (S m) (S n) = leq m n

ev :: Nat -> Bool
ev O = True
ev (S (S m)) = ev m
ev _ = False

od :: Nat -> Bool
od O = False
od (S (S m)) = od m
od _ = True

isMul3 :: Nat -> Bool
isMul3 O = True
isMul3 (S (S (S m))) = isMul3 m
isMul3 _ = False

divides :: Nat -> Nat -> Bool
divides _ O = False
divides m n = rem m n == O

isZero :: Nat -> Bool
isZero O = True
isZero _ = False