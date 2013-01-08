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
import Gemstone.Sprite

data Velocity v = Velocity { _vX, _vY :: v }
    deriving (Show)

makeLenses ''Velocity

data Animation v = Animation { _aSprite   :: Sprite v
                             , _aVelocity :: Velocity v }
    deriving (Show)

makeLenses ''Animation

data Timers = Timers { _tTimestamp :: Word32
                     , _tDelta     :: Word32
                     , _tFps       :: Float }
    deriving (Show)

makeLenses ''Timers

data Gems = Gems { _gScreen    :: Surface
                 , _gCharacter :: Animation GLfloat
                 , _gQuitFlag  :: Bool
                 , _gTimers    :: Timers }
    deriving (Show)

makeLenses ''Gems

gems :: Simple Lens (Gems, a) Gems
gems = _1

data Globals = Globals { _gBall :: Animation GLfloat }

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

makeTimers :: Timers
makeTimers = Timers 0 0 0

getInitialState :: IO Gems
getInitialState = let
    b = BoxLike (-0.9) (-0.9) 0.9 0.9 ^?! box
    in do
    screen <- resizeScreen 1 1
    let anim = makeAnimation $ Colored blue b
    return $ Gems screen anim False makeTimers

makeGlobals :: Globals
makeGlobals = Globals ball
    where
    ball = Animation s v
    v = Velocity 0.1 0.1
    s = Colored black $ makeXYWHValid 0 0 0.1 0.1

coordsAt :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
coordsAt w _ dw dh i = let
    w' = w `div` dw
    (y, x) = i `divMod` w'
    in (x * dw, y * dh)

mma :: Fractional a => a -> a -> a
mma new old = (19 * old + new) / 20

updateTimestamp :: Word32 -> Timers -> Timers
updateTimestamp w t = let
    delta = w - (t ^. tTimestamp)
    fps = 1000 / fromIntegral delta
    in tTimestamp .~ w $ tDelta .~ delta $ tFps %~ mma fps $ t

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
            gems . gCharacter . aSprite . sBox . bY -= 0.1
        KeyDown (Keysym SDLK_UP _ _) ->
            gems . gCharacter . aSprite . sBox . bY += 0.1
        KeyDown (Keysym SDLK_LEFT _ _) ->
            gems . gCharacter . aSprite . sBox . bX -= 0.1
        KeyDown (Keysym SDLK_RIGHT _ _) ->
            gems . gCharacter . aSprite . sBox . bX += 0.1
        _ -> lift . putStrLn $ show event
    -- Continue until all events have been handled.
    when (event /= NoEvent) handleEvents

gravitate :: Loop
gravitate = do
    delta <- use $ gems . gTimers . tDelta
    let dT = realToFrac delta / 1000
    -- Integrate acceleration to get velocity.
    gems . gCharacter . aVelocity . vY -= 9.8 * dT
    y <- use $ gems . gCharacter . aVelocity . vY
    -- Integrate velocity to get position.
    gems . gCharacter . aSprite . sBox . bY += y * dT

mainLoop :: Loop
mainLoop = loop
    where
    bg = Colored white $ BoxLike (-0.9) (-0.9) 0.9 0.9 ^?! box
    loop = do
        ticks <- lift getTicks
        gems . gTimers %= updateTimestamp ticks
        fps <- use $ gems . gTimers . tFps
        delta <- use $ gems . gTimers . tDelta
        lift . putStrLn $ "Ticks: " ++ show delta ++ " (FPS: " ++ show (floor fps) ++ ")"
        handleEvents
        gravitate
        lift clearScreen
        lift . drawSprite $ bg
        Animation ball _ <- _2 . gBall <%= move
        lift . drawSprite $ ball
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
