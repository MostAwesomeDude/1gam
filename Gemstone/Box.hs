{-# LANGUAGE EmptyDataDecls #-}
module Gemstone.Box (
    Box(),
    GoodBox, BadBox,
    pInter, bInter,
    makeBadBox, makeBox, validateBox,
    makeXYWH,
    bTag,
    bLeft, bBottom, bRight, bTop,
    bW, bH, bW', bH',
    bX, bY, bXY,
    scaleBox,
) where

import Control.Lens
import Data.Tagged
import Graphics.Rendering.OpenGL

data Box v = Box (Vertex2 v) (Vertex2 v)
    deriving (Show)

data Good
data Bad

type GoodBox v = Tagged Good (Box v)
type BadBox v = Tagged Bad (Box v)

-- Whether two vertices would determine the lower-left and upper-right corners
-- of a rectangle.
pInter :: Ord v => Vertex2 v -> Vertex2 v -> Bool
pInter (Vertex2 x1 y1) (Vertex2 x2 y2) = x1 < x2 && y1 < y2

-- Whether two boxes intersect.
-- The boxes have to be good.
bInter :: Ord v => GoodBox v -> GoodBox v -> Bool
bInter (Tagged (Box v1 v2)) (Tagged (Box v3 v4)) =
    pInter v1 v4 && pInter v3 v2

-- Make a possibly valid box.
makeBadBox :: Vertex2 v -> Vertex2 v -> BadBox v
makeBadBox v1 v2 = Tagged $ Box v1 v2

-- Validate a box. This will slap you in the face at runtime if you didn't
-- make a good box.
validateBox :: (Ord v, Show v) => BadBox v -> GoodBox v
validateBox b@(Tagged (Box v1 v2))
    | pInter v1 v2 = Tagged $ Box v1 v2
    | otherwise    = error $ "validateBox: Bad box " ++ show b

-- Possibly make a valid box.
makeBox :: (Ord v, Show v) => Vertex2 v -> Vertex2 v -> GoodBox v
makeBox v1 v2 = validateBox $ makeBadBox v1 v2

-- Make a valid box with width and height.
-- This function will slap you hard if you specify zero width or height.
makeXYWH :: (Ord v, Num v) => v -> v -> v -> v -> GoodBox v
makeXYWH x y w h
    | w > 0 && h > 0 = Tagged $ Box (Vertex2 x y) (Vertex2 (x + w) (y + h))
    | otherwise      = error "makeXYWH: Zero width or height"

-- Lens to unwrap and rewrap a good box with its tag. Revalidates the box on
-- setting.
bTag :: (Ord a, Show a) => Simple Lens (GoodBox a) (Box a)
bTag f (Tagged b) = fmap (\b' -> validateBox . Tagged $ b') (f b)

-- Resize a box by moving an edge.
bLeft, bBottom, bRight, bTop :: Simple Lens (Box a) a
bLeft f (Box (Vertex2 x y) v) = fmap (\x' -> Box (Vertex2 x' y) v) (f x)
bBottom f (Box (Vertex2 x y) v) = fmap (\y' -> Box (Vertex2 x y') v) (f y)
bRight f (Box v (Vertex2 x y)) = fmap (\x' -> Box v (Vertex2 x' y)) (f x)
bTop f (Box v (Vertex2 x y)) = fmap (Box v . Vertex2 x)           (f y)

-- Resize a box by changing its width or height.
bW, bH, bW', bH' :: Num a => Simple Lens (Box a) a
bW f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\w -> Box (Vertex2 x y) (Vertex2 (x + w) y')) (f (x' - x))
bH f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\h -> Box (Vertex2 x y) (Vertex2 x' (y + h))) (f (y' - y))
bW' f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\w -> Box (Vertex2 (x' - w) y) (Vertex2 x' y')) (f (x' - x))
bH' f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\h -> Box (Vertex2 x (y' - h)) (Vertex2 x' y)) (f (y' - y))

-- Move a box.
bX, bY :: Num a => Simple Lens (Box a) a
bX f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\w -> Box (Vertex2 w y) (Vertex2 (w + x' - x) y')) (f x)
bY f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\h -> Box (Vertex2 x h) (Vertex2 x' (h + y' - y))) (f y)

-- Move a box more efficiently.
bXY :: Num a => Simple Lens (Box a) (a, a)
bXY f (Box (Vertex2 x y) (Vertex2 x' y')) = let
    f' (w, h) = Box (Vertex2 w h) (Vertex2 (w + x' - x) (h + y' - y))
    in fmap f' (f (x, y))

-- Scale a box.
scaleBox :: (Eq v, Num v) => v -> v -> GoodBox v -> GoodBox v
scaleBox 0 _ _ = error "scaleBox: Zero width"
scaleBox _ 0 _ = error "scaleBox: Zero height"
scaleBox sx sy (Tagged (Box (Vertex2 x1 y1) (Vertex2 x2 y2))) =
    Tagged $ Box (Vertex2 (x1 * sx) (y1 * sy)) (Vertex2 (x2 * sx) (y2 * sy))
