{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Word

import Codec.Image.STB
import Data.Bitmap.Simple
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL

-- Because StateVar chose to override the name "get". :T
getState :: HasGetter g => g a -> IO a
getState = Graphics.Rendering.OpenGL.get

data Box c a = Box (Color3 c) (Vertex2 a) (Vertex2 a)

data GlobalData = GlobalData { _screen    :: Surface
                             , _timestamp :: Word32
                             , _fps       :: Int
                             , _quitFlag  :: Bool }
    deriving (Show)

makeLenses ''GlobalData

type Loop = StateT GlobalData IO ()

bX, bY, bX', bY' :: Simple Lens (Box c a) a
bX f (Box c (Vertex2 x y) v) = fmap (\x' -> Box c (Vertex2 x' y) v) (f x)
bY f (Box c (Vertex2 x y) v) = fmap (\y' -> Box c (Vertex2 x y') v) (f y)
bX' f (Box c v (Vertex2 x y)) = fmap (\x' -> Box c v (Vertex2 x' y)) (f x)
bY' f (Box c v (Vertex2 x y)) = fmap (\y' -> Box c v (Vertex2 x y')) (f y)

bW, bH, bW', bH' :: (Num a) => Simple Lens (Box c a) a
bW f (Box c (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\w -> Box c (Vertex2 x y) (Vertex2 (x + w) y')) (f (x' - x))
bH f (Box c (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\h -> Box c (Vertex2 x y) (Vertex2 x' (y + h))) (f (y' - y))
bW' f (Box c (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\w -> Box c (Vertex2 (x' - w) y) (Vertex2 x' y')) (f (x' - x))
bH' f (Box c (Vertex2 x y) (Vertex2 x' y')) =
    fmap (\h -> Box c (Vertex2 x (y' - h)) (Vertex2 x' y)) (f (y' - y))

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
        matrixMode $= Projection
        ortho2D 0 0 1 1
        return screen

getInitialState :: IO GlobalData
getInitialState = do
    screen <- resizeScreen 1 1
    return $ GlobalData screen 0 0 False

coordsAt :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
coordsAt w h dw dh i = let
    w' = w `div` dw
    (y, x) = i `divMod` w'
    in (x * dw, y * dh)

clearScreen :: IO ()
clearScreen = do
    clearColor $= Color4 0.1 0.1 0.1 0.0
    clear [ColorBuffer]

drawBox :: Box GLubyte GLfloat -> IO ()
drawBox b@(Box c _ _) = renderPrimitive Quads quad
    where
    x = b ^. bX
    y = b ^. bY
    x' = b ^. bX'
    y' = b ^. bY'
    quad = do
        color $ c
        vertex (Vertex2 x y)
        vertex (Vertex2 x' y)
        vertex (Vertex2 x' y')
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
mainLoop = lift loadTexture >> loop
    where
    box = Box (Color3 0 0 255) (Vertex2 0.9 0.9) (Vertex2 (-0.9) (-0.9))
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
        lift . drawBox $ box
        lift finishFrame
        updateTimestamp
        q <- use quitFlag
        unless q loop
    loadTexture = do
        ei <- loadImage "shine1.png"
        let b = case ei of
                Left x  -> error x
                Right x -> x
        withBitmap b $ \(w, h) chans pad ptr -> let
            size = TextureSize2D (fromIntegral w) (fromIntegral h)
            pixels = PixelData RGBA UnsignedByte ptr
            in do
                name:[] <- genObjectNames 1
                textureBinding Texture2D $= Just name
                texImage2D Nothing NoProxy 0 RGBA8 size 0 pixels

checkExtensions :: IO ()
checkExtensions = let
    required = ["ARB_texture_rectangle"]
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
