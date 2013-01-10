module Gemstone.Maths where

-- | Modified Moving Average.
mma :: Fractional a => a -> a -> a
mma new old = (19 * old + new) / 20
