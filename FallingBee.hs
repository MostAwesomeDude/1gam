{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map as M

import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL

import Gemstone.Box
import Gemstone.Color
import Gemstone.GL
import Gemstone.Loop
import Gemstone.Main
import Gemstone.Sprite

type Coords = (Int, Int)

data Globals = Globals { _gTableau :: M.Map Coords Material
                       , _gCurrentCoords :: (Int, Int)
                       , _gCurrentMat :: Material }
    deriving (Show)

makeLenses ''Globals

getInitialState :: IO Globals
getInitialState = return $ Globals M.empty (5, 9) (Colored red Nothing)

matMap :: (Coords, Material) -> Sprite GLfloat
matMap ((x, y), mat) = Sprite mat $ makeXYWHValid x' y' 0.1 0.1
    where
    x' = fromIntegral x * 0.1
    y' = fromIntegral y * 0.1

eventHandler :: Event -> StateT Globals IO ()
eventHandler event = case event of
    NoEvent -> return ()
    _ -> lift . putStrLn $ show event

mainLoop :: Loop Globals
mainLoop = gemstoneLoop pre draw (return ())
    where
    pre :: Loop Globals
    pre = do
        handleEvents eventHandler
    draw :: Loop Globals
    draw = do
        lift clearScreen
        zoom _2 $ do
            zoom gTableau $ do
                sprites <- gets (map matMap . M.toList)
                lift $ drawSprites sprites
            coords <- use gCurrentCoords
            mat <- use gCurrentMat
            lift . drawSprite $ matMap (coords, mat)
        lift finishFrame

main :: IO ()
main = gemstoneMain getInitialState mainLoop
