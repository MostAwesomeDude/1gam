{-# LANGUAGE TemplateHaskell #-}
module Working.Transform where

import Working.Box

data Transform a = Scale a a
                 | Translate a a
    deriving (Show)

-- Apply a transformation to a box, yielding a transformed box.
transform :: Transform a -> GoodBox a -> GoodBox a
transform (Scale sx sy) b = bTag . bX *~ sx $ bTag . bY *~ sy $ b
transform (Translate tx ty) b = bTag . bX +~ sx $ bTag . bY +~ sy $ b
