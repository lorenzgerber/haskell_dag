import Data.List
import Data.Maybe
import Data.Ord
import Control.Arrow
import Data.Char


class Weight w where
    weight :: w -> Int
    plus :: w -> w -> Int

instance Weight Int where
    weight = id
    plus a b = a + b 

instance Weight Char where
    weight a = ord a 
    plus a b = (ord a) + (ord b)


data Year = Year Integer

instance Num Year where
    Year a + Year b = Year (a + b)
    Year a * Year b = Year (a * b)
    abs (Year a) = Year (abs a)
    signum (Year a) = Year (signum a)
    fromInteger a = Year (fromInteger a)
    negate (Year a) = Year (negate a)
    
