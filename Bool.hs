module Bool where

import Predule
    hiding (Bool, true, false)
import Nat

data Bool = True | False
    deriving (Eq, Show)


