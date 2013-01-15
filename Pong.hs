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
import Gemstone.Loop
import Gemstone.Maths
import Gemstone.Main
import Gemstone.Sprite
import Gemstone.Timers

data Velocity v = Velocity { _vX, _vY :: v }
    deriving (Show)

makeLenses ''Velocity

data Animation v = Animation { _aSprite   :: Sprite v
                             , _aVelocity :: Velocity v }
    deriving (Show)

makeLenses ''Animation

data Globals = Globals { _gBall :: Animation GLfloat
                       , _gPaddle :: Animation GLfloat }

makeLenses ''Globals

makeVelocity :: Num v => Velocity v
makeVelocity = Velocity 0 0

makeAnimation :: Num v => Sprite v -> Animation v
makeAnimation s = Animation s makeVelocity

-- | Chop up a duration according to the delta of a timer.
dt :: Fractional a => Timers -> a -> a
dt timers x = delta * x / 1000
    where delta = fromIntegral $ timers ^. tDelta

move :: (Num v, Ord v, Show v) => v -> Animation v -> Animation v
move delta (Animation s v@(Velocity dx dy)) = Animation s' v
    where s' = s & sBox . bXY %~ (\(x, y) -> (x + delta * dx, y + delta * dy))

animate :: Num a => Sprite a -> Animation a
animate s = Animation s $ Velocity 0 0

makeGlobals :: Globals
makeGlobals = Globals ball paddle
    where
    ball = Animation s v
    v = Velocity 0.2 0.2
    s = Colored black $ makeXYWHValid 0.3 0.6 0.1 0.1
    paddle = animate . Colored black $ makeXYWHValid (-0.92) (-0.1) 0.02 0.2

coordsAt :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
coordsAt w _ dw dh i = let
    w' = w `div` dw
    (y, x) = i `divMod` w'
    in (x * dw, y * dh)

eventHandler :: Event -> StateT Globals IO ()
eventHandler event = case event of
    NoEvent -> return ()
    KeyDown (Keysym SDLK_DOWN _ _) ->
        gPaddle . aVelocity . vY .= -1
    KeyDown (Keysym SDLK_UP _ _) ->
        gPaddle . aVelocity . vY .= 1
    KeyUp (Keysym SDLK_DOWN _ _) ->
        gPaddle . aVelocity . vY .= 0
    KeyUp (Keysym SDLK_UP _ _) ->
        gPaddle . aVelocity . vY .= 0
    _ -> lift . putStrLn $ show event

mainLoop :: Loop Globals
mainLoop = loop
    where
    bg = Colored white $ makeXYXYValid (-1) (-1) 1 1
    loop = do
        ticks <- lift getTicks
        gems . gTimers %= updateTimestamp ticks
        fps <- use $ gems . gTimers . tFps
        delta <- use $ gems . gTimers . tDelta
        lift . putStrLn $ "Ticks: " ++ show delta ++ " (FPS: " ++ show (floor fps) ++ ")"
        handleEvents eventHandler
        lift clearScreen
        delta <- uses (gems . gTimers . tDelta) (\x -> fromIntegral x / 1000.0)
        Animation ball _ <- _2 . gBall <%= move delta
        Animation paddle _ <- _2 . gPaddle <%= move delta
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

main :: IO ()
main = gemstoneMain makeGlobals mainLoop
