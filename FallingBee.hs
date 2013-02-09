{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array
import qualified Data.Map as M
import Data.Maybe

import Codec.Image.STB
import Data.Bitmap.OpenGL
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL
import Linear as L

import Gemstone.Animation
import Gemstone.Box
import Gemstone.Color
import Gemstone.GL
import Gemstone.Loop
import Gemstone.Main
import Gemstone.Sprite
import Gemstone.Timers

data Globals = Globals
    deriving (Show)

makeLenses ''Globals

getInitialState :: IO Globals
getInitialState = return Globals

eventHandler :: Event -> StateT Globals IO ()
eventHandler event = case event of
    NoEvent -> return ()
    _ -> lift . putStrLn $ show event

mainLoop :: Loop Globals
mainLoop = gemstoneLoop pre draw (return ())
    where
    pre :: Loop Globals
    pre = handleEvents eventHandler
    draw :: Loop Globals
    draw = do
        lift clearScreen
        lift finishFrame

main :: IO ()
main = gemstoneMain getInitialState mainLoop
