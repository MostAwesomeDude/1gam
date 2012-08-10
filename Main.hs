module Main where

import Control.Monad
import Data.IORef

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image

setScreen :: IO Surface
setScreen = setVideoMode 640 480 32 [DoubleBuf, SWSurface]

mainLoop :: Surface -> IORef Bool -> IO ()
mainLoop screen quit = do
    event <- pollEvent
    case event of
        NoEvent -> return ()
        KeyDown (Keysym SDLK_ESCAPE _ _) -> writeIORef quit True
        _ -> putStrLn $ show event
    SDL.flip screen
    doQuit <- readIORef quit
    unless doQuit $ mainLoop screen quit

actualMain :: IO ()
actualMain = do
    screen <- setScreen
    quit <- newIORef False
    mainLoop screen quit

main :: IO ()
main = withInit [InitEverything] actualMain
