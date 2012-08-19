{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Word

import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL

data Box c = Box (Color3 c) SDL.Rect

data GlobalData = GlobalData { _screen    :: Surface
                             , _timestamp :: Word32
                             , _fps       :: Int
                             , _quitFlag  :: Bool }
    deriving (Show)

makeLenses ''GlobalData

type Loop = StateT GlobalData IO ()

rX, rY, rW, rH :: Simple Lens SDL.Rect Int
rX f r = fmap (\x -> r { rectX = x }) (f (rectX r))
rY f r = fmap (\x -> r { rectY = x }) (f (rectY r))
rW f r = fmap (\x -> r { rectW = x }) (f (rectW r))
rH f r = fmap (\x -> r { rectH = x }) (f (rectH r))

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

drawBox :: (ColorComponent c) => Box c -> IO ()
drawBox (Box c r) = renderPrimitive Quads quad
    where
    -- x = r ^. rX . to fromIntegral :: GLint
    -- y = r ^. rY . to fromIntegral
    -- x' = x + (r ^. rW . to fromIntegral) :: GLint
    -- y' = y + (r ^. rH . to fromIntegral)
    x = -0.9 :: GLfloat
    y = -0.9 :: GLfloat
    x' = 0.9 :: GLfloat
    y' = 0.9 :: GLfloat
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
mainLoop = loop
    where
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
        lift . drawBox $ Box (Color3 0 0 (255 :: GLubyte)) (SDL.Rect 20 30 40 50)
        lift finishFrame
        updateTimestamp
        q <- use quitFlag
        unless q loop

actualMain :: IO ()
actualMain = do
    initial <- getInitialState
    _ <- runStateT mainLoop initial
    return ()

main :: IO ()
main = withInit [InitEverything] actualMain
