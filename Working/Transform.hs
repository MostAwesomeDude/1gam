module Working.Transform where

import Control.Lens

import Working.Box

data Transform a = Scale a a
                 | Translate a a
    deriving (Show)

-- Apply a transformation to a box, yielding a transformed box.
transform :: (Ord a, Num a) => Transform a -> GoodBox a -> GoodBox a
transform (Scale sx sy) b = bTag . bX *~ sx $ bTag . bY *~ sy $ b
transform (Translate tx ty) b = bTag . bX +~ tx $ bTag . bY +~ ty $ b
