module Nat where

import Prelude
    hiding ((+), (*), (^), (-), maximum, minimum, last, (/), (<), (>), (>=), (<=), init, fib, isPrefixOf, drop, take, enumFromTo, 
    reverse, (++), product, sum, elem, length, quot, min, gcd, lcm, div, max, pred, rem, compare)
import Types
import Ordering

(+) :: Nat -> Nat -> Nat
m + O = m
m + (S n) = S (m + n)

(*) :: Nat -> Nat -> Nat
_ * O = O
m * (S n) = m + (m * n)

(^) :: Nat -> Nat -> Nat
_ ^ O = S O
m ^ (S n) = m * (m ^ n)

(-) :: Nat -> Nat -> Nat
m - O = m
m - (S n) = S (m - n)

max :: Nat -> Nat -> Nat
max m O = m
max m (S n) = S (max m n)

min :: Nat -> Nat -> Nat
min _ O = O
min m (S n) = S (min m n)

double :: Nat -> Nat
double O = O
double (S m) = S (S (double m))

fact :: Nat -> Nat
fact O = S O
fact (S m) = S m * fact m

fib :: Nat -> Nat 
fib O = O
fib (S O) = S O
fib (S (S m)) = fib (S m) + fib m

pred :: Nat -> Nat
pred O = O
pred (S m) = m

(/) :: Nat -> Nat -> Nat
m / n
    | m < n = O
    | otherwise = S ((m - n) / m) 

rem :: Nat -> Nat -> Nat
rem m n = m - ((m / n) * n)

-- div :: Nat -> Nat -> (Nat, Nat)
-- div m n
--    | m < n = (O, n)
--    | otherwise = 