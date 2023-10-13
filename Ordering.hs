module Ordering where

import Prelude
    hiding ((<), (>), (<=), (>=), compare)
import Types ( Nat(..) )

compare :: Nat -> Nat -> Ordering
compare (S m) O = GT
compare O (S n) = LT
compare O O = EQ
compare (S m) (S n) = compare m n
    
(<) :: Nat -> Nat -> Bool
m < n = compare m n == LT
    
(>) :: Nat -> Nat -> Bool 
m > n = compare m n == GT
    
(<=) :: Nat -> Nat -> Bool
m <= n = m < n || m == n
    
(>=) :: Nat -> Nat -> Bool
m >= n = m > n || m == n