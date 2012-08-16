{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Map.Lens
import Data.Word

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image

data GlobalData = GlobalData { _screen    :: Surface
                             , _timestamp :: Word32
                             , _quitFlag  :: Bool }
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

getScreen :: IO GlobalData
getScreen = do
    screen <- setVideoMode 640 480 32 [SWSurface, DoubleBuf]
    return $ GlobalData screen 0 False

coordsAt :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
coordsAt w h dw dh i = let
    w' = w `div` dw
    (y, x) = i `divMod` w'
    in (x * dw, y * dh)

clearScreen :: Loop
clearScreen = do
    s <- use screen
    _ <- lift $ fillRect s Nothing (Pixel 0x333333)
    return ()

finishFrame :: Loop
finishFrame = do
    s <- use screen
    lift . SDL.flip $ s

updateTimestamp :: Loop
updateTimestamp = do
    ticks <- use timestamp
    ticks' <- lift getTicks
    let diff = ticks' - ticks
    let fps = 1000 `div` diff
    lift . putStrLn $ "Ticks: " ++ show diff ++ " (FPS: " ++ show fps ++ ")"
    timestamp .= ticks'

mainLoop :: Loop
mainLoop = loop
    where
    loop = do
        event <- lift pollEvent
        case event of
            NoEvent -> return ()
            KeyDown (Keysym SDLK_ESCAPE _ _) -> quitFlag .= True
            _ -> lift . putStrLn $ show event
        clearScreen
        finishFrame
        updateTimestamp
        q <- use quitFlag
        unless q loop

actualMain :: IO ()
actualMain = do
    initial <- getScreen
    _ <- runStateT mainLoop initial
    return ()

main :: IO ()
main = withInit [InitEverything] actualMain
