{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Word

import Codec.Image.STB
import Data.Bitmap.OpenGL
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL

-- Because StateVar chose to override the name "get". :T
getState :: HasGetter g => g a -> IO a
getState = Graphics.Rendering.OpenGL.get

checkErrors :: IO ()
checkErrors = do
    es <- getState errors
    if null es
        then putStrLn "All clear!"
        else putStrLn ("Error: " ++ show es)

data Box v = Box (Vertex2 v) (Vertex2 v)
    deriving (Show)

data Sprite c v = Colored (Color3 c) (Box v)
                | Textured TextureObject (Box v)
    deriving (Show)

data GlobalData = GlobalData { _screen    :: Surface
                             , _timestamp :: Word32
                             , _fps       :: Int
                             , _character :: Sprite GLubyte GLfloat
                             , _quitFlag  :: Bool }
    deriving (Show)

makeLenses ''GlobalData

type Loop = StateT GlobalData IO ()

bX, bY, bX', bY' :: Simple Lens (Box a) a
bX f (Box (Vertex2 x y) v) = fmap (\x' -> Box (Vertex2 x' y) v) (f x)
bY f (Box (Vertex2 x y) v) = fmap (\y' -> Box (Vertex2 x y') v) (f y)
bX' f (Box v (Vertex2 x y)) = fmap (\x' -> Box v (Vertex2 x' y)) (f x)
bY' f (Box v (Vertex2 x y)) = fmap (Box v . Vertex2 x)           (f y)

bW, bH, bW', bH' :: (Num a) => Simple Lens (Box a) a
bW f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\w -> Box (Vertex2 x y) (Vertex2 (x + w) y')) (f (x' - x))
bH f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\h -> Box (Vertex2 x y) (Vertex2 x' (y + h))) (f (y' - y))
bW' f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\w -> Box (Vertex2 (x' - w) y) (Vertex2 x' y')) (f (x' - x))
bH' f (Box (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\h -> Box (Vertex2 x (y' - h)) (Vertex2 x' y)) (f (y' - y))

liift :: (MonadTrans t, Monad m) => (b -> m a) -> b -> t m a
liift = (lift .)

liiift :: (MonadTrans t, Monad m) => (b -> c -> m a) -> b -> c -> t m a
liiift = ((lift .) .)

-- Resize the viewport such that:
--  * The smallest dimension still corresponds to at least [-1, 1]
--  * The viewport is centered on (0, 0)
resizeViewport :: GLsizei -> GLsizei -> IO ()
resizeViewport w h
    | w > h     = viewport $= (Position ((w - h) `div` 2) 0, Size h h)
    | otherwise = viewport $= (Position 0 ((h - w) `div` 2), Size w w)

resizeScreen :: GLsizei -> GLsizei -> IO Surface
resizeScreen w h = let
    flags = [OpenGL, DoubleBuf, Resizable]
    in do
        screen <- setVideoMode (fromIntegral w) (fromIntegral h) 32 flags
        resizeViewport w h
        return screen

getInitialState :: IO GlobalData
getInitialState = do
    screen <- resizeScreen 1 1
    let box = Colored (Color3 0 0 255) $ Box (Vertex2 0.9 0.9) (Vertex2 (-0.9) (-0.9))
    return $ GlobalData screen 0 0 box False

coordsAt :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
coordsAt w h dw dh i = let
    w' = w `div` dw
    (y, x) = i `divMod` w'
    in (x * dw, y * dh)

clearScreen :: IO ()
clearScreen = do
    clearColor $= Color4 0.1 0.1 0.1 0.0
    clear [ColorBuffer]

drawSprite :: Sprite GLubyte GLfloat -> IO ()
drawSprite (Colored c b) = renderPrimitive Quads quad
    where
    x = b ^. bX
    y = b ^. bY
    x' = b ^. bX'
    y' = b ^. bY'
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
    x = b ^. bX
    y = b ^. bY
    x' = b ^. bX'
    y' = b ^. bY'
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

finishFrame :: IO ()
finishFrame = glSwapBuffers

mma :: Int -> Int -> Int
mma new old = (19 * old + new) `div` 20

updateTimestamp :: Loop
updateTimestamp = do
    ticks <- use timestamp
    ticks' <- lift getTicks
    let diff = ticks' - ticks
    let fps' = fromIntegral $ 1000 `div` diff
    adjusted <- fps <%= mma fps'
    lift . putStrLn $ "Ticks: " ++ show diff ++ " (FPS: " ++ show adjusted ++ ")"
    timestamp .= ticks'

mainLoop :: Loop
mainLoop = makeShine >> loop
    where
    makeShine = do
        texobj <- lift . loadTexture $ "shine2.png"
        character .= Textured texobj (Box (Vertex2 0.8 0.8) (Vertex2 (-0.8) (-0.8)))
    box = Colored (Color3 0 0 255) $ Box (Vertex2 0.9 0.9) (Vertex2 (-0.9) (-0.9))
    loop = do
        event <- lift pollEvent
        case event of
            NoEvent -> return ()
            VideoResize w h -> do
                s <- lift $ resizeScreen (fromIntegral w) (fromIntegral h)
                screen .= s
            KeyDown (Keysym SDLK_ESCAPE _ _) -> quitFlag .= True
            _ -> lift . putStrLn $ show event
        lift clearScreen
        lift . drawSprite $ box
        shine <- use character
        lift . drawSprite $ shine
        lift finishFrame
        updateTimestamp
        q <- use quitFlag
        unless q loop

loadTexture :: FilePath -> IO TextureObject
loadTexture path = do
    ei <- loadImage path
    let b = case ei of
            Left x  -> error x
            Right x -> x
    makeSimpleBitmapTexture b

checkExtensions :: IO ()
checkExtensions = let
    required = ["ARB_texture_rectangle", "ARB_texture_non_power_of_two"]
    f x = elem $ "GL_" ++ x
    in do
        exts <- getState glExtensions
        forM_ required $ \x -> if f x exts
            then putStrLn $ "Found extension " ++ x
            else error $ "Need extension " ++ x

actualMain :: IO ()
actualMain = do
    initial <- getInitialState
    checkExtensions
    _ <- runStateT mainLoop initial
    return ()

main :: IO ()
main = withInit [InitEverything] actualMain
