module Working.GL where

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
