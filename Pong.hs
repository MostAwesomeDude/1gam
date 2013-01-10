{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array
import qualified Data.Map as M
import Data.Maybe
import Data.Word

import Codec.Image.STB
import Data.Bitmap.OpenGL
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL

import Gemstone.Box
import Gemstone.Color
import Gemstone.GL
import Gemstone.Maths
import Gemstone.Sprite
import Gemstone.Timers

data Velocity v = Velocity { _vX, _vY :: v }
    deriving (Show)

makeLenses ''Velocity

data Animation v = Animation { _aSprite   :: Sprite v
                             , _aVelocity :: Velocity v }
    deriving (Show)

makeLenses ''Animation

data Gems = Gems { _gScreen    :: Surface
                 , _gCharacter :: Animation GLfloat
                 , _gQuitFlag  :: Bool
                 , _gTimers    :: Timers }
    deriving (Show)

makeLenses ''Gems

gems :: Simple Lens (Gems, a) Gems
gems = _1

data Globals = Globals { _gBall :: Animation GLfloat
                       , _gPaddle :: Animation GLfloat }

makeLenses ''Globals

type Loop = StateT (Gems, Globals) IO ()

resizeScreen :: GLsizei -> GLsizei -> IO Surface
resizeScreen w h = let
    flags = [OpenGL, DoubleBuf, Resizable]
    in do
        screen <- setVideoMode (fromIntegral w) (fromIntegral h) 32 flags
        resizeViewport w h
        return screen

makeVelocity :: Num v => Velocity v
makeVelocity = Velocity 0 0

makeAnimation :: Num v => Sprite v -> Animation v
makeAnimation s = Animation s makeVelocity

move :: (Num v, Ord v, Show v) => Animation v -> Animation v
move (Animation s v@(Velocity dx dy)) = Animation s' v
    where s' = s & sBox . bXY %~ (\(x, y) -> (x + dx, y + dy))

getInitialState :: IO Gems
getInitialState = let
    b = makeXYXYValid (-0.9) (-0.9) 0.9 0.9
    in do
    screen <- resizeScreen 1 1
    let anim = makeAnimation $ Colored blue b
    return $ Gems screen anim False makeTimers

animate :: Num a => Sprite a -> Animation a
animate s = Animation s $ Velocity 0 0

makeGlobals :: Globals
makeGlobals = Globals ball paddle
    where
    ball = Animation s v
    v = Velocity 0.05 0.05
    s = Colored black $ makeXYWHValid 0.3 0.6 0.1 0.1
    paddle = animate . (Colored black) $ makeXYWHValid (-0.88) (-0.1) 0.01 0.2

coordsAt :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
coordsAt w _ dw dh i = let
    w' = w `div` dw
    (y, x) = i `divMod` w'
    in (x * dw, y * dh)

handleEvent :: Event -> Gems -> Gems
handleEvent (KeyDown (Keysym SDLK_ESCAPE _ _)) = gQuitFlag .~ True
handleEvent _ = id

handleEvents :: Loop
handleEvents = do
    event <- lift pollEvent
    gems %= handleEvent event
    case event of
        NoEvent -> return ()
        VideoResize w h ->
            gems . gScreen <~ lift (resizeScreen (fromIntegral w) (fromIntegral h))
        KeyDown (Keysym SDLK_DOWN _ _) ->
            _2 . gPaddle . aSprite . sBox . bY -= 0.05
        KeyDown (Keysym SDLK_UP _ _) ->
            _2 . gPaddle . aSprite . sBox . bY += 0.05
        _ -> lift . putStrLn $ show event
    -- Continue until all events have been handled.
    when (event /= NoEvent) handleEvents

mainLoop :: Loop
mainLoop = loop
    where
    bg = Colored white $ makeXYXYValid (-0.9) (-0.9) 0.9 0.9
    loop = do
        ticks <- lift getTicks
        gems . gTimers %= updateTimestamp ticks
        fps <- use $ gems . gTimers . tFps
        delta <- use $ gems . gTimers . tDelta
        lift . putStrLn $ "Ticks: " ++ show delta ++ " (FPS: " ++ show (floor fps) ++ ")"
        handleEvents
        lift clearScreen
        Animation ball _ <- _2 . gBall <%= move
        Animation paddle _ <- use $ _2 . gPaddle
        zoom (_2 . gBall) $ do
            y <- uses (aSprite . sBox . bY) $ \x -> abs x >= 0.9
            when y $ aVelocity .vY %= negate
            x <- uses (aSprite . sBox . bX) $ \x -> abs x >= 0.9
            paddled <- uses (aSprite . sBox) $ \b -> bInter b $ paddle ^. sBox
            when (x || paddled) $ aVelocity .vX %= negate
        lift . drawSprites $ [bg, ball, paddle]
        lift finishFrame
        q <- use $ gems . gQuitFlag
        unless q loop

actualMain :: IO ()
actualMain = do
    initial <- getInitialState
    checkExtensions
    _ <- runStateT mainLoop (initial, makeGlobals)
    return ()

main :: IO ()
main = withInit [InitEverything] actualMain
