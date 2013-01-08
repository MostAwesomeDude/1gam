{-# LANGUAGE EmptyDataDecls, TemplateHaskell #-}
module Gemstone.Box (
    Box(), BoxLike(..), unBox, box,
    pInter, bInter,
    makeXYWH,
    bLeft, bBottom, bRight, bTop,
    bW, bH, bW', bH',
    bX, bY, bXY,
    scaleBox,
) where

import Control.Lens

data BoxLike v = BoxLike { _bx1, _by1, _bx2, _by2 :: v }
    deriving (Show)

makeLenses ''BoxLike

newtype Box v = Box { unBox :: BoxLike v }
    deriving (Show)

-- | Whether two vertices would determine the lower-left and upper-right corners
--   of a rectangle.
--
--   This is the predicate for determining whether a box is valid.
pInter :: Ord v => v -> v -> v -> v -> Bool
pInter x1 y1 x2 y2 = x1 < x2 && y1 < y2

-- | Prism for getting a Box from a BoxLike.
--
--   The Box constructor isn't exported, so this is the only way to obtain a
--   valid Box.
box :: Ord v => Simple Prism (BoxLike v) (Box v)
box = prism unBox f
    where
    f b | pred b = Right $ Box b
        | otherwise = Left b
    pred (BoxLike x1 y1 x2 y2) = pInter x1 y1 x2 y2

-- | Whether two boxes intersect.
--
--   The boxes have to be good.
bInter :: Ord v => Box v -> Box v -> Bool
bInter (Box b1) (Box b2) =
    pInter (b1 ^. bx1) (b1 ^. by1) (b2 ^. bx2) (b2 ^. by2) &&
    pInter (b2 ^. bx1) (b2 ^. by1) (b1 ^. bx2) (b1 ^. by2)

-- | Make a box with width and height.
makeXYWH :: (Ord v, Num v) => v -> v -> v -> v -> BoxLike v
makeXYWH x y w h = BoxLike x y (x + w) (y + h)

-- | Like makeXYWH, but valid.
--
--   Includes a slap on the face if the box is not valid.
makeXYWHValid :: (Ord v, Num v) => v -> v -> v -> v -> Box v
makeXYWHValid x y w h = case makeXYWH x y w h ^? box of
    Just b -> b
    Nothing -> error "makeXYWHValid: Zero width or height"

-- | Resize a box by moving an edge.
--
--   Inherently unsafe.
bLeft, bBottom, bRight, bTop :: Simple Lens (BoxLike a) a
bLeft   = bx1
bBottom = by1
bRight  = bx2
bTop    = by2

-- | Resize a box by changing its width or height.
--
--   Inherently unsafe.
bW, bH, bW', bH' :: Num a => Simple Lens (BoxLike a) a
bW  f (BoxLike x1 y1 x2 y2) =
    fmap (\w -> BoxLike x1 y1 (x2 + w) y2) (f (x2 - x1))
bH  f (BoxLike x1 y1 x2 y2) =
    fmap (\h -> BoxLike x1 y1 x2 (y2 + h)) (f (y2 - y1))
bW' f (BoxLike x1 y1 x2 y2) =
    fmap (\w -> BoxLike (x1 - w) y1 x2 y2) (f (x2 - x1))
bH' f (BoxLike x1 y1 x2 y2) =
    fmap (\h -> BoxLike x1 (y1 - h) x2 y2) (f (y2 - y1))

-- | Move a box.
bX, bY :: Num a => Simple Lens (Box a) a
bX f (Box (BoxLike x1 y1 x2 y2)) =
    fmap (\x' -> Box (BoxLike x' y1 (x' + x2 - x1) y2)) (f x1)
bY f (Box (BoxLike x1 y1 x2 y2)) =
    fmap (\y' -> Box (BoxLike x1 y1 x2 (y' + y2 - y1))) (f y1)

-- Move a box more efficiently.
bXY :: Num a => Simple Lens (Box a) (a, a)
bXY f (Box (BoxLike x1 y1 x2 y2)) = let
    f' (w, h) = Box $ BoxLike w h (w + x2 - x1) (h + y2 - y1)
    in fmap f' $ f (x1, y1)

-- Scale a box.
scaleBox :: (Eq v, Num v) => v -> v -> Box v -> Box v
scaleBox 0 _ _ = error "scaleBox: Zero width"
scaleBox _ 0 _ = error "scaleBox: Zero height"
scaleBox sx sy (Box (BoxLike x1 y1 x2 y2)) =
    Box $ BoxLike (x1 * sx) (y1 * sy) (x2 * sx) (y2 * sy)
