{-# LANGUAGE TemplateHaskell #-}
-- Copyright (C) 2014 Google Inc. All rights reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain
-- a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied. See the License for the specific language governing
-- permissions and limitations under the License.
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Codec.Image.STB
import Data.Bitmap.OpenGL
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

paddleSprites :: Simple Traversal Paddle (Sprite GLfloat)
paddleSprites f (Paddle a s t) =
    Paddle <$> aSprite f a <*> pure s <*> particleSprites f t

data Globals = Globals { _gFont :: Font
                       , _gBall :: Animation GLfloat
                       , _gPlayer :: Paddle
                       , _gCPU :: Paddle
                       , _gPaused :: Bool
                       , _gShowFPS :: Bool
                       , _gParticles :: Particles GLfloat }

makeLenses ''Globals

globalSprites :: Simple Traversal Globals (Sprite GLfloat)
globalSprites f (Globals t b p1 p2 p s ps) =
    Globals t <$> aSprite f b <*> paddleSprites f p1 <*>
    paddleSprites f p2 <*> pure p <*> pure s <*> particleSprites f ps

textBox :: GLfloat -> GLfloat -> GLfloat -> Box GLfloat
textBox x y h = makeXYWHValid x y 0.1 h

makePaddle :: GLfloat -> Paddle
makePaddle x = Paddle s 0 ps
    where
    s = animate . colored black $ makeXYWHValid x 0.45 0.02 0.1
    ps = makeParticles & pColor .~ Color3 20 20 20 & pColorVariance .~ 20

loadTexture :: FilePath -> IO TextureObject
loadTexture path = do
    ei <- loadImage path
    let b = case ei of
            Left x  -> error x
            Right x -> x
    makeSimpleBitmapTexture b

makeGlobals :: IO Globals
makeGlobals = do
    font <- createPolygonFont "Inconsolata.otf"
    ballTexture <- loadTexture "ball.png"
    let ball = newBall $ Textured ballTexture
    void $ setFontFaceSize font 2 72
    return $ Globals font ball player cpu False True ballParticles
    where
    ballParticles = makeParticles & pColor .~ Color3 235 20 20 & pColorVariance .~ 20
    player = makePaddle 0.08
    cpu = makePaddle 0.90

newBall :: Material -> Animation GLfloat
newBall material = Animation s 0.2
    where s = Sprite material $ makeXYWHValid 0.3 0.3 0.05 0.05

resetBall :: Globals -> Globals
resetBall g = g & gBall .~ newBall material
    where material = g ^. gBall . aSprite . sMaterial

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

halfway :: Fractional a => (a, a) -> a
halfway (x, y) = (x + y) / 2

showScores :: Globals -> IO ()
showScores g = do
    drawSprite $ Sprite (Text font playerScore blue) playerBox
    drawSprite $ Sprite (Text font cpuScore blue) cpuBox
    where
    font = g ^. gFont
    playerScore = g ^. gPlayer . pScore . to show
    cpuScore = g ^. gCPU . pScore . to show
    playerBox = textBox 0.2 0.7 0.1
    cpuBox = textBox 0.7 0.7 0.1

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

render :: Gems -> Globals -> IO ()
render gems g = do
    -- Draw the background, then the scores, and then the ball and
    -- players.
    drawSprite bg
    showScores g
    mapMOf_ globalSprites drawSprite g
    when (g ^. gPaused) $ do
        drawSprite $ Sprite (Colored black (Just 127)) (makeXYWHValid 0 0 1 (1 :: GLfloat))
        drawSprite $ Sprite (Text font "PAUSED" blue) (textBox 0.2 0.45 0.2)
    when (g ^. gShowFPS) $ do
        drawSprite $ Sprite (Text font fpsText blue) (textBox 0.9 0.97 0.02)
    where
    bg = colored white $ makeXYXYValid 0 0 1 (1 :: GLfloat)
    font = g ^. gFont
    fps = gems ^. gTimers . tFps
    fpsText = "FPS: " ++ show (floor fps :: Int)

mainLoop :: Loop Globals
mainLoop = gemstoneLoop pre draw (return ())
    where
    pre = do
        handleEvents eventHandler
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
    draw = do
        lift clearScreen
        (gems, g) <- use id
        lift $ render gems g
        lift finishFrame

main :: IO ()
main = gemstoneMain makeGlobals mainLoop
