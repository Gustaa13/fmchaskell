module Nat where

import Prelude
  hiding ((-), last, (<), (>), (>=), (<=), compare, init, isPrefixOf, maximum, minimum, drop, take, enumFromTo, reverse, (++), product, sum, elem, length, (+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem)

data Nat = O | S Nat
    deriving ( Eq, Show )

