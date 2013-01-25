{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array
import qualified Data.Map as M
import Data.Maybe

import Codec.Image.STB
import Data.Bitmap.OpenGL
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL
import Linear as L

import Gemstone.Animation
import Gemstone.Box
import Gemstone.Color
import Gemstone.GL
import Gemstone.Loop
import Gemstone.Main
import Gemstone.Sprite
import Gemstone.Timers

data RawTile = Off | On
    deriving (Eq, Enum, Ord, Show)

data Tile = Sky | Ground | Impass
    deriving (Eq, Ord, Show)

tileColors :: M.Map Tile RGB
tileColors = M.fromList [ (Sky, skyBlue)
                        , (Ground, grassGreen)
                        , (Impass, stoneGray) ]

type RawTiles = Array (Int, Int) RawTile
type Tiles = Array (Int, Int) Tile

basicTiles :: RawTiles
basicTiles = array ((0, 0), (15, 15)) xs
    where
    coords = [(x, y) | y <- [0..15], x <- [0..15]]
    stuff = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0
            ,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0
            ,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0
            ,0,0,0,0,0,1,1,1,0,1,1,1,0,0,0,0
            ,0,1,1,0,0,1,1,1,0,1,1,1,0,0,0,0
            ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
    xs = zip (reverse coords) (map toEnum stuff)

-- | Center, Left, Right, Down, Up
convolve :: RawTile -> RawTile -> RawTile -> RawTile -> RawTile -> Tile
convolve On  On On On On = Impass
convolve On  _  _  _  _  = Ground
convolve Off _  _  _  _  = Sky

colorTiles :: RawTiles -> Tiles
colorTiles rt = let
    bounds' = bounds rt
    check i e = if inRange bounds' i then e else On
    l i = check i $ rt ! i
    convolved (x, y) = convolve (l (x, y)) (l (x - 1, y)) (l (x + 1, y)) (l (x, y - 1)) (l (x, y + 1))
    in array bounds' [((x, y), convolved (x, y)) | (x, y) <- range bounds']

data Globals = Globals { _gCharacter :: Animation GLfloat
                       , _gTiles     :: RawTiles
                       , _gShowTiles :: Bool }
    deriving (Show)

makeLenses ''Globals

getInitialState :: Globals
getInitialState = let
    b = makeXYXYValid 0.1 0.1 0.9 0.9
    anim = animate $ colored blue b
    in Globals anim basicTiles True

coordsAt :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
coordsAt w _ dw dh i = let
    w' = w `div` dw
    (y, x) = i `divMod` w'
    in (x * dw, y * dh)

drawTile :: (Num v, Real v) => (v, v) -> RGB -> IO ()
drawTile (x, y) c = let
    b = makeXYWHValid (realToFrac x) (realToFrac y) 1 (1 :: GLfloat)
    b' = scaleBox (1/16) (1/16) b
    in drawSprite $ colored c b'

drawRawTiles :: RawTiles -> IO ()
drawRawTiles t = forM_ (assocs t) $ \((x, y), tile) -> let
    c = if tile == On then green else red
    in drawTile (x, y) c

drawTiles :: Tiles -> IO ()
drawTiles t = forM_ (assocs t) $ \((x, y), tile) -> let
    c = fromMaybe white $ M.lookup tile tileColors
    in drawTile (x, y) c

eventHandler :: Event -> StateT Globals IO ()
eventHandler event = case event of
    NoEvent -> return ()
    KeyDown (Keysym SDLK_DOWN _ _) ->
        gCharacter . aSprite . sBox . bY -= 0.1
    KeyDown (Keysym SDLK_UP _ _) ->
        gCharacter . aSprite . sBox . bY += 0.1
    KeyDown (Keysym SDLK_LEFT _ _) ->
        gCharacter . aSprite . sBox . bX -= 0.1
    KeyDown (Keysym SDLK_RIGHT _ _) ->
        gCharacter . aSprite . sBox . bX += 0.1
    KeyDown (Keysym SDLK_t _ _) ->
        gShowTiles %= not
    _ -> lift . putStrLn $ show event

gravitate :: Loop Globals
gravitate = do
    delta <- use $ gems . gTimers . tDelta
    let dT = realToFrac delta / 1000
    -- Integrate acceleration to get velocity.
    _2 . gCharacter . aVelocity . _y -= 9.8 * dT
    y <- use $ _2 . gCharacter . aVelocity . _y
    -- Integrate velocity to get position.
    _2 . gCharacter . aSprite . sBox . bY += y * dT

mainLoop :: Loop Globals
mainLoop = makeShine >> loop
    where
    makeShine :: Loop Globals
    makeShine = let
        b = makeXYXYValid 0.7 0.7 0.8 0.8
        in do
        texobj <- lift . loadTexture $ "shine2.png"
        _2 . gCharacter .= animate (Sprite (Textured texobj) b)
    loop :: Loop Globals
    loop = do
        ticks <- lift getTicks
        gems . gTimers %= updateTimestamp ticks
        fps <- use $ gems . gTimers . tFps
        delta <- use $ gems . gTimers . tDelta
        lift . putStrLn $ "Ticks: " ++ show delta ++ " (FPS: " ++ show (floor fps :: Int) ++ ")"
        handleEvents eventHandler
        -- gravitate
        lift clearScreen
        whether <- use $ _2 . gShowTiles
        tiles <- use $ _2 . gTiles
        if whether
            then lift . drawTiles . colorTiles $ tiles
            else lift . drawRawTiles $ tiles
        shine <- use $ _2 . gCharacter . aSprite
        lift . drawSprite $ shine
        lift finishFrame
        q <- use $ gems . gQuitFlag
        unless q loop

loadTexture :: FilePath -> IO TextureObject
loadTexture path = do
    ei <- loadImage path
    let b = case ei of
            Left x  -> error x
            Right x -> x
    makeSimpleBitmapTexture b

main :: IO ()
main = gemstoneMain getInitialState mainLoop
