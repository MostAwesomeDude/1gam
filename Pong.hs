{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Graphics.Rendering.FTGL as FTGL
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.SDL as SDL
import Linear as L

import Gemstone.Animation
import Gemstone.Box
import Gemstone.Color
import Gemstone.GL
import Gemstone.Loop
import Gemstone.Main
import Gemstone.Particles
import Gemstone.Sprite
import Gemstone.Timers

data Paddle = Paddle { _pPaddle :: Animation GLfloat
                     , _pScore :: Int
                     , _pTrail :: Particles GLfloat }

makeLenses ''Paddle

data Globals = Globals { _gFont :: Font
                       , _gBall :: Animation GLfloat
                       , _gPlayer :: Paddle
                       , _gCPU :: Paddle
                       , _gPaused :: Bool
                       , _gShowFPS :: Bool
                       , _gParticles :: Particles GLfloat }

makeLenses ''Globals

data Text a = Text String RGB a a a
    deriving (Show)

makePaddle :: GLfloat -> Paddle
makePaddle x = Paddle s 0 ps
    where
    s = animate . colored black $ makeXYWHValid x 0.45 0.02 0.1
    ps = makeParticles & pColor .~ Color3 20 20 20 & pColorVariance .~ 20

makeGlobals :: IO Globals
makeGlobals = do
    font <- createPolygonFont "Inconsolata.otf"
    void $ setFontFaceSize font 2 72
    return $ Globals font newBall player cpu False True ballParticles
    where
    ballParticles = makeParticles & pColor .~ Color3 235 20 20 & pColorVariance .~ 20
    player = makePaddle 0.08
    cpu = makePaddle 0.90

newBall :: Animation GLfloat
newBall = Animation s 0.2
    where s = colored red $ makeXYWHValid 0.3 0.3 0.05 0.05

resetBall :: Globals -> Globals
resetBall = gBall .~ newBall

eventHandler :: Event -> StateT Globals IO ()
eventHandler event = case event of
    NoEvent -> return ()
    KeyDown (Keysym SDLK_SPACE _ _) -> gPaused %= not
    KeyDown (Keysym SDLK_TAB _ _) -> gShowFPS %= not
    KeyDown (Keysym SDLK_DOWN _ _) -> gPlayer . pPaddle . aVelocity . _y .= -0.3
    KeyDown (Keysym SDLK_UP _ _) -> gPlayer . pPaddle . aVelocity . _y .= 0.3
    KeyUp (Keysym SDLK_DOWN _ _) -> gPlayer . pPaddle . aVelocity . _y .= 0
    KeyUp (Keysym SDLK_UP _ _) -> gPlayer . pPaddle . aVelocity . _y .= 0
    _ -> lift . putStrLn $ show event

-- | Write some text.
write :: (Fractional c, Num c, MatrixComponent c) => Font -> Text c -> IO ()
write font (Text text c x y h) = let h' = h / 2 in do
    color c
    preservingMatrix $ do
        translate $ Vector3 x y 0
        scale h' h' 1
        renderFont font text All

halfway :: Fractional a => (a, a) -> a
halfway (x, y) = (x + y) / 2

showScores :: StateT Globals IO ()
showScores = do
    font <- use gFont
    playerScore <- uses (gPlayer . pScore) show
    cpuScore <- uses (gCPU . pScore) show
    lift $ write font (Text playerScore blue 0.2 0.7 (0.1 :: GLfloat))
    lift $ write font (Text cpuScore blue 0.7 0.7 (0.1 :: GLfloat))

clampPaddle :: (Ord a, Num a) => Sprite a -> Sprite a
clampPaddle s = s & sBox . bY %~ max 0 & sBox . bY' %~ min 1

aimBall :: (Epsilon a, Fractional a, Num a, RealFloat a) =>
            Box a -> Box a -> V2 a -> V2 a
aimBall paddle ball previous = let
    (px, py) = center paddle
    (bx, by) = center ball
    direction = L.normalize $ V2 bx by - V2 px py
    mag = norm previous + 0.01
    in direction * realToFrac mag

-- Any paddle collision should successfully get the ball heading the other
-- direction, regardless of intersection depth; this is to prevent situations
-- where the ball might get stuck inside the paddle, negating every frame but
-- never breaking free.
paddleBall :: Box GLfloat -> StateT Globals IO ()
paddleBall b = do
    paddled <- uses (gBall . aSprite . sBox) $ bInter b
    when paddled $ do
        ballBox <- use $ gBall . aSprite . sBox
        gBall . aVelocity %= aimBall b ballBox

mainLoop :: Loop Globals
mainLoop = loop
    where
    bg = colored white $ makeXYXYValid 0 0 1 (1 :: GLfloat)
    loop = do
        ticks <- lift getTicks
        gems . gTimers %= updateTimestamp ticks
        handleEvents eventHandler
        lift clearScreen
        paused <- use $ _2 . gPaused
        unless paused $ do
            delta <- use $ gems . gTimers . tDelta
            let dt = fromIntegral delta / 1000.0
            zoom _2 $ forM_ [gBall, gPlayer . pPaddle, gCPU . pPaddle] $
                \l -> l %= moveAnimation dt
            _2 . gPlayer . pPaddle . aSprite %= clampPaddle
            _2 . gCPU . pPaddle . aSprite %= clampPaddle
            coords <- use $ _2 . gBall . aSprite . sBox . to center
            zoom (_2 . gParticles) $ do
                pCenter .= coords
                id %= tickParticles (fromIntegral delta)
            zoom (_2 . gPlayer) $ do
                pTrail . pCenter <~ use (pPaddle . aSprite . sBox . to center)
                pTrail %= tickParticles (fromIntegral delta)
            zoom (_2 . gCPU) $ do
                pTrail . pCenter <~ use (pPaddle . aSprite . sBox . to center)
                pTrail %= tickParticles (fromIntegral delta)
        -- Move the CPU's paddle towards the ball.
        zoom _2 $ do
            first <- use $ gBall . aSprite . sBox . remit box . bBot
            second <- use $ gBall . aSprite . sBox . remit box . bTop
            third <- use $ gCPU . pPaddle . aSprite . sBox . remit box . bBot
            fourth <- use $ gCPU . pPaddle . aSprite . sBox . remit box . bTop
            let midpoint = halfway (first, second)
                current = halfway (third, fourth)
            gCPU . pPaddle . aVelocity . _y .= if abs (midpoint - current) > 0.01
                then if midpoint <= current
                    then (-0.3)
                    else 0.3
                else 0
        unless paused $ zoom _2 $ do
            pScored <- uses (gBall . aSprite . sBox . remit box . bRight) (>= 1)
            when pScored $ modify resetBall >> gPlayer . pScore += 1
            cScored <- uses (gBall . aSprite . sBox . remit box . bLeft) (<= 0)
            when cScored $ modify resetBall >> gCPU . pScore += 1
            -- Now, check for the top and bottom bounds of the arena.
            collidesBot <- uses (gBall . aSprite . sBox . remit box . bBot) (<= 0)
            when collidesBot $ gBall . aVelocity . _y %= abs
            collidesTop <- uses (gBall . aSprite . sBox . remit box . bTop) (>= 1)
            when collidesTop $ gBall . aVelocity . _y %= negate . abs
            -- Then check for collisions with the paddle.
            paddleBox <- use $ gPlayer . pPaddle . aSprite . sBox
            paddleBall paddleBox
            cpuBox <- use $ gCPU . pPaddle . aSprite . sBox
            paddleBall cpuBox
        -- Draw the background, then the scores, and then the ball and
        -- players.
        lift $ drawSprite bg
        zoom _2 showScores
        -- Particles are behind the ball and paddles.
        forM_ [gParticles, gPlayer . pTrail, gCPU . pTrail] $ \l -> do
            particles <- use $ _2 . l . pParticles
            forM_ particles $ \p -> lift $ drawSprite (p ^. _1 . aSprite)
        forM_ [gBall, gPlayer . pPaddle, gCPU . pPaddle] $ \l -> do
            sprite <- use $ _2 . l . aSprite
            lift $ drawSprite sprite
        when paused $ do
            font <- use $ _2 . gFont
            lift . drawSprite $ Sprite (Colored black (Just 127)) (makeXYWHValid 0 0 1 (1 :: GLfloat))
            lift $ write font (Text "PAUSED" blue 0.2 0.45 (0.2 :: GLfloat))
        showFPS <- use $ _2 . gShowFPS
        when showFPS $ do
            font <- use $ _2 . gFont
            fps <- use $ gems . gTimers . tFps
            lift $ write font (Text ("FPS: " ++ show (floor fps :: Int)) blue 0.9 0.97 (0.02 :: GLfloat))
        lift finishFrame
        q <- use $ gems . gQuitFlag
        unless q loop

main :: IO ()
main = do
    globals <- makeGlobals
    gemstoneMain globals mainLoop
