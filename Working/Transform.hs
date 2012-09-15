{-# LANGUAGE TemplateHaskell #-}
module Working.Transform where

import Control.Lens

import Working.Box

data Transform a = Transform { _tFactorX, _tFactorY :: a
                             , _tOffsetX, _tOffsetY :: a }
    deriving (Show)

makeLenses ''Transform

-- Define a transformation from one box to another.
transformRef :: (Fractional a, Ord a) => GoodBox a -> GoodBox a -> Transform a
transformRef src dest = let
    origin b = bX .~ 0 $ bY .~ 0 $ b
    usrc = src ^. bTag
    udest = dest ^. bTag
    -- Grab the offsets. bBottom and bLeft would work here too.
    ox = udest ^. bX - usrc ^. bX
    oy = udest ^. bY - usrc ^. bY
    -- Move the boxes so that their bottom-left corners are at the origin.
    -- This'll make it easy to calculate the factors.
    src' = origin usrc
    dest' = origin udest
    fx = dest' ^. bRight / src' ^. bRight
    fy = dest' ^. bTop / src' ^. bTop
    in Transform fx fy ox oy

-- Apply a transformation to a box, yielding a transformed box.
transform :: (Fractional a, Ord a) => Transform a -> GoodBox a -> GoodBox a
transform (Transform fx fy ox oy) b = let
    -- First, the translation...
    trans b = bTag . bX +~ ox $ bTag . bY +~ oy $ b
    -- ...then, the scaling.
    scale b = bTag . bW *~ fx $ bTag . bH *~ fy $ b
    in scale . trans $ b
