module Order where

import Prelude
    hiding ((<), (>), (<=), (>=), compare)
import Nat

data Order = GT | LT | EQ
    deriving (Eq, Show)

compare :: Nat -> Nat -> Order
compare _ O = GT
compare O _ = LT
compare n n = EQ
compare (S m) (S n) = compare m n
    
(<) :: Nat -> Nat -> Bool
m < n = compare m n == LT
    
(>) :: Nat -> Nat -> Bool 
m > n = compare m n == GT
    
(<=) :: Nat -> Nat -> Bool
m <= n = m < n || compare m n == EQ
    
(>=) :: Nat -> Nat -> Bool
m >= n = m > n || compare m n == EQ