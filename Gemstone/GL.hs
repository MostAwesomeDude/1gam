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
--    * The smallest dimension still corresponds to at least [-1, 1]
--    * The viewport is centered on (0, 0)
resizeViewport :: GLsizei -> GLsizei -> IO ()
resizeViewport w h
    | w > h     = viewport $= (Position ((w - h) `div` 2) 0, Size h h)
    | otherwise = viewport $= (Position 0 ((h - w) `div` 2), Size w w)
