{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Map.Lens

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image

data Sprite = Sprite { _sAtlas :: Surface
                     , _rect :: Rect }
    deriving (Show)

makeLenses ''Sprite

data SpriteSheet = SpriteSheet { _ssFilePath :: FilePath
                               , _ssAtlas :: Surface
                               , _ssw, _ssh, _ssdw, _ssdh :: Int }
    deriving (Show)

makeLenses ''SpriteSheet

type ImageMap = Map.Map FilePath SpriteSheet

data GlobalData = GlobalData { _screen   :: Surface
                             , _images   :: ImageMap
                             , _sprite   :: Sprite
                             , _quitFlag :: Bool }
    deriving (Show)

makeLenses ''GlobalData

type Loop = StateT GlobalData IO ()

rX, rY, rW, rH :: Simple Lens Rect Int
rX f r = fmap (\x -> r { rectX = x }) (f (rectX r))
rY f r = fmap (\x -> r { rectY = x }) (f (rectY r))
rW f r = fmap (\x -> r { rectW = x }) (f (rectW r))
rH f r = fmap (\x -> r { rectH = x }) (f (rectH r))

liift :: (MonadTrans t, Monad m) => (b -> m a) -> b -> t m a
liift = (lift .)

liiift :: (MonadTrans t, Monad m) => (b -> c -> m a) -> b -> c -> t m a
liiift = ((lift .) .)

nullSprite :: IO Sprite
nullSprite = do
    s <- createRGBSurface [SWSurface] 0xff 0x0 0x0 0 0 100 100
    s' <- displayFormat s
    r <- getClipRect s'
    return $ Sprite s' r

getScreen :: IO GlobalData
getScreen = do
    screen <- setVideoMode 640 480 32 [SWSurface, DoubleBuf]
    spr <- nullSprite
    return $ GlobalData screen Map.empty spr False

coordsAt :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
coordsAt w h dw dh i = let
    w' = w `div` dw
    (y, x) = i `divMod` w'
    in (x * dw, y * dh)

toSprite :: Int -> Getter SpriteSheet Sprite
toSprite i = let
    f (SpriteSheet _ atlas w h dw dh) = let
        (x', y') = coordsAt w h dw dh i
        in Sprite atlas $ Rect x' y' (x' + dw) (y' + dh)
    in to f

spriteAt :: String -> Int -> Getter ImageMap Sprite
spriteAt s i = let
    f m = case m ^. at s of
        Just sheet -> sheet ^. toSprite i
        Nothing -> undefined
    in to f

loadImage :: FilePath -> ImageMap -> IO ImageMap
loadImage path m = do
    surface <- load path
    rect <- getClipRect surface
    let w = rectW rect
        h = rectH rect
        sheet = SpriteSheet path surface w h 408 325
    return $ Map.insert path sheet m

loadSheet :: FilePath -> Loop
loadSheet path = do
    m <- use images
    images <~ liiift loadImage path m

clearScreen :: Loop
clearScreen = do
    s <- use screen
    _ <- lift $ fillRect s Nothing (Pixel 0x333333)
    return ()

blitSprite :: Sprite -> Loop
blitSprite sprite = let
    atlas = sprite ^. sAtlas
    sr = sprite ^. rect
    in do
        s <- use screen
        lift . putStrLn $ show sprite
        _ <- lift $ blitSurface atlas (Just sr) s Nothing
        return ()

finishFrame :: Loop
finishFrame = do
    s <- use screen
    lift . SDL.flip $ s

mainLoop :: Loop
mainLoop = loadImages >> loop
    where
    loop = do
        event <- lift pollEvent
        case event of
            NoEvent -> return ()
            KeyDown (Keysym SDLK_ESCAPE _ _) -> quitFlag .= True
            KeyDown (Keysym SDLK_LEFT _ _) -> sprite . rect . rX -= 1
            KeyDown (Keysym SDLK_RIGHT _ _) -> sprite . rect . rX += 1
            KeyDown (Keysym SDLK_UP _ _) -> sprite . rect . rY -= 1
            KeyDown (Keysym SDLK_DOWN _ _) -> sprite . rect . rY += 1
            _ -> lift . putStrLn $ show event
        spr <- use sprite
        clearScreen
        blitSprite spr
        finishFrame
        q <- use quitFlag
        unless q loop
    loadImages = do
        loadSheet "heather1.png"
        spr <- use $ images . spriteAt "heather1.png" 0
        sprite .= spr

actualMain :: IO ()
actualMain = do
    initial <- getScreen
    runStateT mainLoop initial
    return ()

main :: IO ()
main = withInit [InitEverything] actualMain
