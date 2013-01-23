{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Random

import Graphics.Rendering.FTGL as FTGL
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.SDL as SDL
import Linear as L

import Gemstone.Box
import Gemstone.Color
import Gemstone.GL
import Gemstone.Loop
import Gemstone.Main
import Gemstone.Sprite
import Gemstone.Timers

data Animation v = Animation { _aSprite   :: Sprite v
                             , _aVelocity :: V2 v }
    deriving (Show)

makeLenses ''Animation

data Particle v = Particle { _pAnimation :: Animation v
                           , _pTicks :: Int }
    deriving (Show)

makeLenses ''Particle

data Particles v = Particles { _pGen :: StdGen
                             , _pCenter :: (v, v)
                             , _pParticles :: [Particle v] }

makeLenses ''Particles

data Globals = Globals { _gFont :: Font
                       , _gBall :: Animation GLfloat
                       , _gPlayer :: Animation GLfloat
                       , _gCPU :: Animation GLfloat
                       , _gPlayerScore :: Int
                       , _gCPUScore :: Int
                       , _gBounces :: Int
                       , _gPaused :: Bool
                       , _gParticles :: Particles GLfloat }

makeLenses ''Globals

data Text a = Text String RGB a a a
    deriving (Show)

makeAnimation :: Num v => Sprite v -> Animation v
makeAnimation s = Animation s 0

makeParticle :: (Floating v, Ord v) => (v, v) -> Int -> Particle v
makeParticle (x, y) ticks = Particle (makeAnimation s) ticks
    where s = colored green $ makeXYXYValid (x - 0.005) (y - 0.005) (x + 0.005) (y + 0.005)

updateParticles :: (Ord v, Num v) => Int -> [Particle v] -> [Particle v]
updateParticles ticks =
    filter (^. pTicks . to (> 0)) . over (traverse . pTicks) (\x -> x - ticks)

tickParticles :: (Floating v, Ord v) => Int -> Particles v -> Particles v
tickParticles ticks (Particles g center ps) = Particles g' center ps''
    where
    ps' = updateParticles ticks ps
    ps'' = if length ps < 50 then newParticle : ps' else ps'
    (life, g') = randomR (0, 750) g
    newParticle = makeParticle center life

makeParticles :: Num v => Particles v
makeParticles = Particles (mkStdGen 0) (0, 0) []

-- | Chop up a duration according to the delta of a timer.
dt :: Fractional a => Timers -> a -> a
dt timers x = delta * x / 1000
    where delta = fromIntegral $ timers ^. tDelta

move :: (Num v, Ord v, Show v) => v -> Animation v -> Animation v
move delta animation = animation & aSprite . sBox . bXY %~ f
    where
    v' = animation ^. aVelocity * pure delta
    f (x, y) = case V2 x y + v' of V2 x' y' -> (x', y')

animate :: Num a => Sprite a -> Animation a
animate s = Animation s 0

colored :: RGB -> Box v -> Sprite v
colored c b = Sprite (Colored c Nothing) b

makeGlobals :: IO Globals
makeGlobals = do
    font <- createPolygonFont "Inconsolata.otf"
    void $ setFontFaceSize font 1 72
    return $ Globals font ball player cpu 0 0 0 False makeParticles
    where
    ball = Animation s v
    v = 0.2
    s = colored red $ makeXYWHValid 0.3 0.6 0.05 0.05
    player = animate . colored black $ makeXYWHValid 0.08 0.45 0.02 0.1
    cpu = animate . colored black $ makeXYWHValid 0.90 0.45 0.02 0.1

resetBall :: Globals -> Globals
resetBall globals =
    globals & gBall . aSprite . sBox .~ makeXYWHValid 0.3 0.6 0.05 0.05

eventHandler :: Event -> StateT Globals IO ()
eventHandler event = case event of
    NoEvent -> return ()
    KeyDown (Keysym SDLK_SPACE _ _) -> gPaused %= not
    KeyDown (Keysym SDLK_DOWN _ _) -> gPlayer . aVelocity . _y .= -0.3
    KeyDown (Keysym SDLK_UP _ _) -> gPlayer . aVelocity . _y .= 0.3
    KeyUp (Keysym SDLK_DOWN _ _) -> gPlayer . aVelocity . _y .= 0
    KeyUp (Keysym SDLK_UP _ _) -> gPlayer . aVelocity . _y .= 0
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

clampPaddle :: (Ord a, Num a) => Sprite a -> Sprite a
clampPaddle s = s & sBox . bY %~ max 0 & sBox . bY' %~ min 1

aimBall :: (Epsilon a, Fractional a, Num a, RealFloat a) =>
            Int -> Box a -> Box a -> V2 a
aimBall count paddle ball = let
    (px, py) = center paddle
    (bx, by) = center ball
    mag = 0.3 + (0.01 * fromIntegral count)
    scaled = L.normalize $ V2 bx by - V2 px py
    in scaled * mag

-- Any paddle collision should successfully get the ball heading the other
-- direction, regardless of intersection depth; this is to prevent situations
-- where the ball might get stuck inside the paddle, negating every frame but
-- never breaking free.
paddleBall :: Box GLfloat -> StateT Globals IO ()
paddleBall b = do
    paddled <- uses (gBall . aSprite . sBox) $ bInter b
    when paddled $ do
        count <- gBounces <+= 1
        ballBox <- use $ gBall . aSprite . sBox
        gBall . aVelocity .= aimBall count b ballBox

mainLoop :: Loop Globals
mainLoop = loop
    where
    bg = colored white $ makeXYXYValid 0 0 1 (1 :: GLfloat)
    loop = do
        ticks <- lift getTicks
        gems . gTimers %= updateTimestamp ticks
        fps <- use $ gems . gTimers . tFps
        delta <- use $ gems . gTimers . tDelta
        lift . putStrLn $ "Ticks: " ++ show delta ++ " (FPS: " ++ show (floor fps :: Int) ++ ")"
        handleEvents eventHandler
        lift clearScreen
        paused <- use $ _2 . gPaused
        unless paused $ do
            delta' <- uses (gems . gTimers . tDelta) (\x -> fromIntegral x / 1000.0)
            zoom _2 $ forM_ [gBall, gPlayer, gCPU] $ \l -> l %= move delta'
            _2 . gPlayer . aSprite %= clampPaddle
            _2 . gCPU . aSprite %= clampPaddle
            coords <- use $ _2 . gBall . aSprite . sBox . to center
            zoom (_2 . gParticles) $ do
                pCenter .= coords
                id %= tickParticles (fromIntegral delta)
        -- Move the CPU's paddle towards the ball.
        first <- use $ _2 . gBall . aSprite . sBox . remit box . bBot
        second <- use $ _2 . gBall . aSprite . sBox . remit box . bTop
        third <- use $ _2 . gCPU . aSprite . sBox . remit box . bBot
        fourth <- use $ _2 . gCPU . aSprite . sBox . remit box . bTop
        let midpoint = halfway (first, second)
            current = halfway (third, fourth)
        _2 . gCPU . aVelocity . _y .= if midpoint <= current
            then (-0.3)
            else 0.3
        unless paused $ zoom _2 $ do
            pScored <- uses (gBall . aSprite . sBox . remit box . bRight) (>= 1)
            when pScored $ modify resetBall >> gPlayerScore += 1
            cScored <- uses (gBall . aSprite . sBox . remit box . bLeft) (<= 0)
            when cScored $ modify resetBall >> gCPUScore += 1
            -- Now, check for the top and bottom bounds of the arena.
            collidesBot <- uses (gBall . aSprite . sBox . remit box . bBot) (<= 0)
            when collidesBot $ gBall . aVelocity . _y %= abs
            collidesTop <- uses (gBall . aSprite . sBox . remit box . bTop) (>= 1)
            when collidesTop $ gBall . aVelocity . _y %= negate . abs
            -- Then check for collisions with the paddle.
            paddleBox <- use $ gPlayer . aSprite . sBox
            paddleBall paddleBox
            cpuBox <- use $ gCPU . aSprite . sBox
            paddleBall cpuBox
        -- Draw the background, then the scores, and then the ball and
        -- players.
        lift $ drawSprite bg
        zoom _2 showScores
        -- Particles are behind the ball and paddles.
        particles <- use $ _2 . gParticles . pParticles
        forM_ particles $ \p -> lift $ drawSprite (p ^. pAnimation . aSprite)
        forM_ [gBall, gPlayer, gCPU] $ \l -> do
            sprite <- use $ _2 . l . aSprite
            lift $ drawSprite sprite
        when paused $ do
            font <- use $ _2 . gFont
            lift . drawSprite $ Sprite (Colored black (Just 127)) (makeXYWHValid 0 0 1 (1 :: GLfloat))
            lift $ write font (Text "PAUSED" blue 0.2 0.45 (0.2 :: GLfloat))
        lift finishFrame
        q <- use $ gems . gQuitFlag
        unless q loop

main :: IO ()
main = do
    globals <- makeGlobals
    gemstoneMain globals mainLoop
