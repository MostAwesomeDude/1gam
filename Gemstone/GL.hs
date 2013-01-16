module Gemstone.GL where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL

checkErrors :: IO ()
checkErrors = do
    es <- get errors
    if null es
        then putStrLn "All clear!"
        else putStrLn ("Error: " ++ show es)

checkExtensions :: IO ()
checkExtensions = let
    required = ["ARB_texture_rectangle", "ARB_texture_non_power_of_two"]
    f x = elem $ "GL_" ++ x
    in do
        exts <- get glExtensions
        forM_ required $ \x -> if f x exts
            then putStrLn $ "Found extension " ++ x
            else error $ "Need extension " ++ x

clearScreen :: IO ()
clearScreen = do
    clearColor $= Color4 0.1 0.1 0.1 0.0
    clear [ColorBuffer]

finishFrame :: IO ()
finishFrame = glSwapBuffers

-- | Resize the viewport such that:
--    * The smallest dimension still corresponds to at least [0, 1]
--    * The viewport is centered on (0.5, 0.5)
resizeViewport :: GLsizei -> GLsizei -> IO ()
resizeViewport w h
    | w > h     = viewport $= (Position ((w-3*h) `div` 2) (-h), Size (2*h) (2*h))
    | otherwise = viewport $= (Position (-w) ((h-3*w) `div` 2), Size (2*w) (2*w))
