{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array
import Data.Complex
import Data.Complex.Lens
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
                       , _gCPU :: Animation GLfloat
                       , _gPlayerScore :: Int
                       , _gCPUScore :: Int }

makeLenses ''Globals

data Text a = Text String RGB a a a
    deriving (Show)

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
    return $ Globals font ball player cpu 0 0
    where
    ball = Animation s v
    v = Velocity 0.2 0.2
    s = Colored red $ makeXYWHValid 0.3 0.6 0.1 0.1
    player = animate . Colored black $ makeXYWHValid (0.08) 0.4 0.02 0.2
    cpu = animate . Colored black $ makeXYWHValid 0.90 0.4 0.02 0.2

resetBall :: Globals -> Globals
resetBall globals =
    globals & gBall . aSprite . sBox .~ makeXYWHValid 0.3 0.6 0.1 0.1

eventHandler :: Event -> StateT Globals IO ()
eventHandler event = case event of
    NoEvent -> return ()
    KeyDown (Keysym SDLK_DOWN _ _) ->
        gPlayer . aVelocity . vY .= -0.3
    KeyDown (Keysym SDLK_UP _ _) ->
        gPlayer . aVelocity . vY .= 0.3
    KeyUp (Keysym SDLK_DOWN _ _) ->
        gPlayer . aVelocity . vY .= 0
    KeyUp (Keysym SDLK_UP _ _) ->
        gPlayer . aVelocity . vY .= 0
    _ -> lift . putStrLn $ show event

-- | Write some text.
write :: (Num c, MatrixComponent c) => Font -> Text c -> IO ()
write font (Text text c x y h) = do
    color c
    preservingMatrix $ do
        translate $ Vector3 x y 0
        scale h h 1
        renderFont font text All

halfway :: Fractional a => (a, a) -> a
halfway (x, y) = (x + y) / 2

showScores :: StateT Globals IO ()
showScores = do
    font <- use gFont
    pScore <- uses gPlayerScore show
    cScore <- uses gCPUScore show
    lift $ write font (Text pScore blue 0.2 0.7 (0.1 :: GLfloat))
    lift $ write font (Text cScore blue 0.7 0.7 (0.1 :: GLfloat))

aimBall :: (Fractional a, Num a, RealFloat a) => Box a -> Box a -> Velocity a
aimBall paddle ball = let
    (px, py) = center paddle
    (bx, by) = center ball
    vx :+ vy = (bx - px) :+ (by - py) & _magnitude .~ 0.3
    in Velocity vx vy

mainLoop :: Loop Globals
mainLoop = loop
    where
    bg = Colored white $ makeXYXYValid 0 0 1 1
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
        -- Move the CPU's paddle towards the ball.
        first <- use $ _2 . gBall . aSprite . sBox . remit box . bBot
        second <- use $ _2 . gBall . aSprite . sBox . remit box . bTop
        third <- use $ _2 . gCPU . aSprite . sBox . remit box . bBot
        fourth <- use $ _2 . gCPU . aSprite . sBox . remit box . bTop
        let midpoint = halfway (first, second)
            current = halfway (third, fourth)
        _2 . gCPU . aVelocity . vY .= if midpoint <= current
            then (-0.3)
            else 0.3
        zoom _2 $ do
            pScored <- uses (gBall . aSprite . sBox . remit box . bRight) $ (>= 1)
            when pScored $ modify resetBall >> gPlayerScore += 1
            cScored <- uses (gBall . aSprite . sBox . remit box . bLeft) $ (<= 0)
            when cScored $ modify resetBall >> gCPUScore += 1
        zoom (_2 . gBall) $ do
            -- First, check for the top and bottom bounds of the arena.
            collidesBot <- uses (aSprite . sBox . remit box . bBot) $ (<= 0)
            when collidesBot $ aVelocity . vY %= abs
            collidesTop <- uses (aSprite . sBox . remit box . bTop) $ (>= 1)
            when collidesTop $ aVelocity . vY %= negate . abs
            -- Then check for collisions with the paddle. Any paddle collision
            -- should successfully get the ball heading the other direction,
            -- regardless of intersection depth; this is to prevent situations
            -- where the ball might get stuck inside the paddle, negating
            -- every frame but never breaking free.
            paddled <- uses (aSprite . sBox) $ \b -> bInter b $ player ^. sBox
            when paddled $ aVelocity .= aimBall (player ^. sBox) (ball ^. sBox)
            paddled <- uses (aSprite . sBox) $ \b -> bInter b $ cpu ^. sBox
            when paddled $ aVelocity .= aimBall (cpu ^. sBox) (ball ^. sBox)
        -- Draw the background, then the scores, and then the ball and
        -- players.
        lift . drawSprite $ bg
        zoom _2 showScores
        lift . drawSprites $ [ball, player, cpu]
        lift finishFrame
        q <- use $ gems . gQuitFlag
        unless q loop

main :: IO ()
main = do
    globals <- makeGlobals
    gemstoneMain globals mainLoop
