module Working.Box where

import Control.Lens
import Graphics.Rendering.OpenGL

data Box v = Box (Vertex2 v) (Vertex2 v)
    deriving (Show)

data Good = Good | Bad
    deriving (Show)

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
