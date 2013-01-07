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

data Sprite v = Colored RGB (GoodBox v)
              | Textured TextureObject (GoodBox v)
    deriving (Show)

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

data Globals = Globals { _gScreen    :: Surface
                       , _gCharacter :: Animation GLfloat
                       , _gTiles     :: RawTiles
                       , _gShowTiles :: Bool
                       , _gQuitFlag  :: Bool
                       , _gTimers    :: Timers }
    deriving (Show)

makeLenses ''Globals

type Loop = StateT Globals IO ()

sBox :: Simple Lens (Sprite v) (GoodBox v)
sBox f (Colored c b) = fmap (Colored c) (f b)
sBox f (Textured t b) = fmap (Textured t) (f b)

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

makeTimers :: Timers
makeTimers = Timers 0 0 0

getInitialState :: IO Globals
getInitialState = let
    box = makeBox (Vertex2 (-0.9) (-0.9)) (Vertex2 0.9 0.9)
    in do
    screen <- resizeScreen 1 1
    let anim = makeAnimation $ Colored blue box
    return $ Globals screen anim basicTiles True False makeTimers

coordsAt :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
coordsAt w _ dw dh i = let
    w' = w `div` dw
    (y, x) = i `divMod` w'
    in (x * dw, y * dh)

drawSprite :: Sprite GLfloat -> IO ()
drawSprite (Colored c b) = renderPrimitive Quads quad
    where
    x = b ^. bTag . bLeft
    y = b ^. bTag . bBottom
    x' = b ^. bTag . bRight
    y' = b ^. bTag . bTop
    quad = do
        color c
        vertex (Vertex2 x y)
        vertex (Vertex2 x' y)
        vertex (Vertex2 x' y')
        vertex (Vertex2 x y')
drawSprite (Textured texobj b) = do
    enableTextures
    renderPrimitive Quads quad
    disableTextures
    where
    x = b ^. bTag . bLeft
    y = b ^. bTag . bBottom
    x' = b ^. bTag . bRight
    y' = b ^. bTag . bTop
    r = 0 :: GLfloat
    s = 0 :: GLfloat
    r' = 1
    s' = 1
    enableTextures = do
        texture Texture2D $= Enabled
        activeTexture $= TextureUnit 0
        textureBinding Texture2D $= Just texobj
        textureFunction $= Replace
    disableTextures = texture Texture2D $= Disabled
    quad = do
        texCoord (TexCoord2 r s)
        vertex (Vertex2 x y)
        texCoord (TexCoord2 r' s)
        vertex (Vertex2 x' y)
        texCoord (TexCoord2 r' s')
        vertex (Vertex2 x' y')
        texCoord (TexCoord2 r s')
        vertex (Vertex2 x y')

drawTile :: (Num v, Real v) => (v, v) -> RGB -> IO ()
drawTile (x, y) c = let
    f (x, y) = (x - 1, y - 1)
    box = makeXYWH (realToFrac x) (realToFrac y) 1 1
    box' = scaleBox (1/8) (1/8) box
    box'' = bTag . bXY %~ f $ box'
    in drawSprite $ Colored c box''

drawRawTiles :: RawTiles -> IO ()
drawRawTiles t = forM_ (assocs t) $ \((x, y), tile) -> let
    c = if tile == On then green else red
    in drawTile (x, y) c

drawTiles :: Tiles -> IO ()
drawTiles t = forM_ (assocs t) $ \((x, y), tile) -> let
    c = fromMaybe white $ M.lookup tile tileColors
    in drawTile (x, y) c

mma :: Fractional a => a -> a -> a
mma new old = (19 * old + new) / 20

updateTimestamp :: Word32 -> Timers -> Timers
updateTimestamp w t = let
    delta = w - (t ^. tTimestamp)
    fps = 1000 / fromIntegral delta
    in tTimestamp .~ w $ tDelta .~ delta $ tFps %~ mma fps $ t

handleEvent :: Event -> Globals -> Globals
handleEvent (KeyDown (Keysym SDLK_ESCAPE _ _)) = gQuitFlag .~ True
handleEvent (KeyDown (Keysym SDLK_t _ _)) = gShowTiles %~ not
handleEvent _ = id

handleEvents :: Loop
handleEvents = do
    event <- lift pollEvent
    modify $ handleEvent event
    case event of
        NoEvent -> return ()
        VideoResize w h -> do
            gScreen <~ lift (resizeScreen (fromIntegral w) (fromIntegral h))
        KeyDown (Keysym SDLK_DOWN _ _) ->
            gCharacter . aSprite . sBox . bTag . bY -= 0.1
        KeyDown (Keysym SDLK_UP _ _) ->
            gCharacter . aSprite . sBox . bTag . bY += 0.1
        KeyDown (Keysym SDLK_LEFT _ _) ->
            gCharacter . aSprite . sBox . bTag . bX -= 0.1
        KeyDown (Keysym SDLK_RIGHT _ _) ->
            gCharacter . aSprite . sBox . bTag . bX += 0.1
        _ -> lift . putStrLn $ show event
    -- Continue until all events have been handled.
    when (event /= NoEvent) handleEvents

gravitate :: Loop
gravitate = do
    delta <- use $ gTimers . tDelta
    let dT = realToFrac delta / 1000
    -- Integrate acceleration to get velocity.
    gCharacter . aVelocity . vY -= 9.8 * dT
    y <- use $ gCharacter . aVelocity . vY
    -- Integrate velocity to get position.
    gCharacter . aSprite . sBox . bTag . bY += y * dT

mainLoop :: Loop
mainLoop = makeShine >> loop
    where
    makeShine = let
        box = makeBox (Vertex2 0.7 0.7) (Vertex2 0.8 0.8)
        in do
        texobj <- lift . loadTexture $ "shine2.png"
        gCharacter .= makeAnimation (Textured texobj box)
    box = Colored blue $ makeBox (Vertex2 (-0.9) (-0.9)) (Vertex2 0.9 0.9)
    loop = do
        ticks <- lift getTicks
        gTimers %= updateTimestamp ticks
        fps <- use $ gTimers . tFps
        delta <- use $ gTimers . tDelta
        lift . putStrLn $ "Ticks: " ++ show delta ++ " (FPS: " ++ show (floor fps) ++ ")"
        handleEvents
        gravitate
        lift clearScreen
        lift . drawSprite $ box
        whether <- use gShowTiles
        tiles <- use gTiles
        if whether
            then lift . drawTiles . colorTiles $ tiles
            else lift . drawRawTiles $ tiles
        shine <- use $ gCharacter . aSprite
        lift . drawSprite $ shine
        lift finishFrame
        q <- use gQuitFlag
        unless q loop

loadTexture :: FilePath -> IO TextureObject
loadTexture path = do
    ei <- loadImage path
    let b = case ei of
            Left x  -> error x
            Right x -> x
    makeSimpleBitmapTexture b

actualMain :: IO ()
actualMain = do
    initial <- getInitialState
    checkExtensions
    _ <- runStateT mainLoop initial
    return ()

main :: IO ()
main = withInit [InitEverything] actualMain
