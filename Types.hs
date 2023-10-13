module Types where

data Nat = O | S Nat
    deriving (Eq, Show)

data ListNat = Empty | Cons ListNat
    deriving (Eq, Show)
