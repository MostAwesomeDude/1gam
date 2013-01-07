module Gemstone.Sprite where

import Control.Lens
import Graphics.Rendering.OpenGL

import Gemstone.Box
import Gemstone.Color

data Sprite v = Colored RGB (GoodBox v)
              | Textured TextureObject (GoodBox v)
    deriving (Show)

sBox :: Simple Lens (Sprite v) (GoodBox v)
sBox f (Colored c b) = fmap (Colored c) (f b)
sBox f (Textured t b) = fmap (Textured t) (f b)

drawSprite :: Sprite GLfloat -> IO ()
drawSprite (Colored c b) = renderPrimitive Quads quad
    where
    x = b ^. bTag . bLeft
    y = b ^. bTag . bBottom
    x' = b ^. bTag . bRight
    y' = b ^. bTag . bTop
    quad = do
        color c
        vertex (Vertex2 x y)
        vertex (Vertex2 x' y)
        vertex (Vertex2 x' y')
        vertex (Vertex2 x y')
drawSprite (Textured texobj b) = do
    enableTextures
    renderPrimitive Quads quad
    disableTextures
    where
    x = b ^. bTag . bLeft
    y = b ^. bTag . bBottom
    x' = b ^. bTag . bRight
    y' = b ^. bTag . bTop
    r = 0 :: GLfloat
    s = 0 :: GLfloat
    r' = 1
    s' = 1
    enableTextures = do
        texture Texture2D $= Enabled
        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just texobj
        textureFunction $= Replace
    disableTextures = texture Texture2D $= Disabled
    quad = do
        texCoord (TexCoord2 r s)
        vertex (Vertex2 x y)
        texCoord (TexCoord2 r' s)
        vertex (Vertex2 x' y)
        texCoord (TexCoord2 r' s')
        vertex (Vertex2 x' y')
        texCoord (TexCoord2 r s')
        vertex (Vertex2 x y')
