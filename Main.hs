module Main where

import Graphics.UI.SDL

mainLoop :: IO ()
mainLoop = return ()

main :: IO ()
main = do
    withInit [InitEverything] mainLoop
