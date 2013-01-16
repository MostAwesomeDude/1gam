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
import Graphics.Rendering.FTGL as FTGL
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

data Globals = Globals { _gFont :: Font
                       , _gBall :: Animation GLfloat
                       , _gPlayer :: Animation GLfloat
                       , _gCPU :: Animation GLfloat }

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

makeGlobals :: IO Globals
makeGlobals = do
    font <- createPolygonFont "Inconsolata.otf"
    setFontFaceSize font 1 72
    return $ Globals font ball player cpu
    where
    ball = Animation s v
    v = Velocity 0.2 0.2
    s = Colored red $ makeXYWHValid 0.3 0.6 0.1 0.1
    player = animate . Colored black $ makeXYWHValid (-0.92) (-0.1) 0.02 0.2
    cpu = animate . Colored black $ makeXYWHValid 0.90 (-0.1) 0.02 0.2

eventHandler :: Event -> StateT Globals IO ()
eventHandler event = case event of
    NoEvent -> return ()
    KeyDown (Keysym SDLK_DOWN _ _) ->
        gPlayer . aVelocity . vY .= -1
    KeyDown (Keysym SDLK_UP _ _) ->
        gPlayer . aVelocity . vY .= 1
    KeyUp (Keysym SDLK_DOWN _ _) ->
        gPlayer . aVelocity . vY .= 0
    KeyUp (Keysym SDLK_UP _ _) ->
        gPlayer . aVelocity . vY .= 0
    _ -> lift . putStrLn $ show event

write :: Font -> String -> RGB -> IO ()
write font text c = do
    color c
    preservingMatrix $ do
        translate $ Vector3 (-0.1) (-0.1) (0 :: GLfloat)
        renderFont font text All

mainLoop :: Loop Globals
mainLoop = loop
    where
    bg = Colored blue $ makeXYXYValid (-1) (-1) 1 1
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
        Animation player _ <- _2 . gPlayer <%= move delta
        Animation cpu _ <- _2 . gCPU <%= move delta
        zoom (_2 . gBall) $ do
            -- First, check for the top and bottom bounds of the arena.
            collidesBot <- uses (aSprite . sBox . remit box . bBot) $ (<= (-1))
            when collidesBot $ aVelocity . vY %= abs
            collidesTop <- uses (aSprite . sBox . remit box . bTop) $ (>= 1)
            when collidesTop $ aVelocity . vY %= negate . abs
            -- And the right-hand side, for now.
            x <- uses (aSprite . sBox . remit box . bRight) $ (>= 1)
            when x $ aVelocity . vX %= negate . abs
            -- Then check for collisions with the paddle. Any paddle collision
            -- should successfully get the ball heading the other direction,
            -- regardless of intersection depth; this is to prevent situations
            -- where the ball might get stuck inside the paddle, negating
            -- every frame but never breaking free.
            paddled <- uses (aSprite . sBox) $ \b -> bInter b $ player ^. sBox
            when paddled $ aVelocity . vX %= abs
        lift . drawSprites $ [bg, ball, player, cpu]
        font <- use $ _2 . gFont
        lift $ write font "Derp" white
        lift finishFrame
        q <- use $ gems . gQuitFlag
        unless q loop

main :: IO ()
main = do
    globals <- makeGlobals
    gemstoneMain globals mainLoop
