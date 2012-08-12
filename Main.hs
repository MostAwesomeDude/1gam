{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.IORef
import qualified Data.Map as Map

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image

data SpriteSheet = SpriteSheet { _ssFilePath :: FilePath
                               , _ssAtlas :: Surface
                               , _ssdw, _ssdh, _ssw, _ssh :: Int }
    deriving (Show)

makeLenses ''SpriteSheet

type ImageMap = Map.Map FilePath SpriteSheet

data GlobalData = GlobalData { _screen   :: Surface
                             , _images   :: ImageMap
                             , _quitFlag :: Bool }
    deriving (Show)

makeLenses ''GlobalData

type Loop = StateT GlobalData IO ()

data Sprite = Sprite { _sAtlas :: Surface
                     , _rect :: Rect }
    deriving (Show)

makeLenses ''Sprite

getScreen :: IO GlobalData
getScreen = do
    screen <- setVideoMode 640 480 32 [DoubleBuf, SWSurface]
    return $ GlobalData screen Map.empty False

coordsAt :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
coordsAt x y w h i = let
    w' = x `div` w
    (y', x') = i `divMod` w'
    in (x', y')

toSprite :: Int -> Getter SpriteSheet Sprite
toSprite i = let
    f (SpriteSheet _ atlas w h x y) = let
        (x', y') = coordsAt x y w h i
        in Sprite atlas $ Rect x' y' (x' + x) (y' + y)
    in to f

loadImage :: FilePath -> ImageMap -> IO ImageMap
loadImage path m = do
    surface <- load path
    rect <- getClipRect surface
    let w = rectW rect
        h = rectH rect
        sheet = SpriteSheet path surface 408 325 w h
    return $ Map.insert path sheet m

loadSheet :: FilePath -> Loop
loadSheet path = do
    m <- use images
    m' <- lift $ loadImage path m
    images .= m'

clearScreen :: Loop
clearScreen = do
    s <- use screen
    _ <- lift $ fillRect s Nothing (Pixel 0x0)
    return ()

finishFrame :: Loop
finishFrame = do
    s <- use screen
    lift $ SDL.flip s

mainLoop :: Loop
mainLoop = loadImages >> loop
    where
    loop = do
        event <- lift pollEvent
        case event of
            NoEvent -> return ()
            KeyDown (Keysym SDLK_ESCAPE _ _) -> quitFlag .= True
            _ -> lift . putStrLn $ show event
        clearScreen
        finishFrame
        q <- use quitFlag
        unless q $ loop
    loadImages = do
        loadSheet "heather1.png"

actualMain :: IO ()
actualMain = do
    initial <- getScreen
    runStateT mainLoop initial
    return ()

main :: IO ()
main = withInit [InitEverything] actualMain
