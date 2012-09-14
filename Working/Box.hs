module Working.Box(
    Box(),
    pInter,
    makeBadBox, makeBox, unwrapBox,
    bLeft, bBottom, bRight, bTop,
    bW, bH, bW', bH',
    bX, bY,
) where

import Control.Lens
import Data.Maybe
import Data.Tagged
import Graphics.Rendering.OpenGL

data Box v = Box (Vertex2 v) (Vertex2 v)
    deriving (Show)

data Good = Good
    deriving (Show)

data Bad = Bad
    deriving (Show)

-- Whether two vertices would determine the lower-left and upper-right corners
-- of a rectangle.
pInter :: Ord v => Vertex2 v -> Vertex2 v -> Bool
pInter (Vertex2 x1 y1) (Vertex2 x2 y2) = x1 < x2 && y1 < y2

-- Make a possibly valid box.
makeBadBox :: Vertex2 v -> Vertex2 v -> Tagged Bad (Box v)
makeBadBox v1 v2 = Tagged $ Box v1 v2

-- Validate a box.
validateBox :: Ord v => Tagged Bad (Box v) -> Maybe (Tagged Good (Box v))
validateBox (Tagged (Box v1 v2)) | pInter v1 v2 = Just . Tagged $ Box v1 v2
                                 | otherwise    = Nothing

-- Possibly make a valid box.
makeBox :: Ord v => Vertex2 v -> Vertex2 v -> Maybe (Tagged Good (Box v))
makeBox v1 v2 = validateBox $ makeBadBox v1 v2

-- Unwrap a box. This will slap you in the face at runtime if you didn't make
-- a good box.
unwrapBox :: Maybe (Tagged Good (Box v)) -> Tagged Good (Box v)
unwrapBox = fromJust

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
