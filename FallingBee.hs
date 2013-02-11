{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map as M
import Data.Word

import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL

import Gemstone.Box
import Gemstone.Color
import Gemstone.GL
import Gemstone.Loop
import Gemstone.Main
import Gemstone.Sprite
import Gemstone.Timers

type Coords = (Int, Int)
type BlockMap = M.Map Coords Material

data Globals = Globals { _gTableau :: M.Map Coords Material
                       , _gCurrentColumn :: Int
                       , _gCurrentRow :: GLfloat
                       , _gCurrentMat :: Material
                       , _gMats :: [RGB] }
    deriving (Show)

makeLenses ''Globals

getInitialState :: IO Globals
getInitialState = return $ Globals M.empty 5 0.9 (Colored blue Nothing) mats
    where mats = cycle [red, green, blue]

matMap :: (Coords, Material) -> Sprite GLfloat
matMap ((x, y), mat) = Sprite mat $ makeXYWHValid x' y' 0.1 0.1
    where
    x' = fromIntegral x * 0.1
    y' = fromIntegral y * 0.1

toRow :: GLfloat -> Int
toRow x = floor $ x * 10

collides :: Int -> Int -> BlockMap -> Bool
collides column row = M.member (column, row)

gCollides :: Globals -> Bool
gCollides g = row < 0 || collides (g ^. gCurrentColumn) row (g ^. gTableau)
    where row = g ^. gCurrentRow . to toRow

unitsPerSecond :: Word32 -> GLfloat
unitsPerSecond t = fromIntegral t / 1000

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
        delta <- use $ gems . gTimers . tDelta
        zoom _2 $ do
            gCurrentRow -= unitsPerSecond delta * 0.1
            column <- use gCurrentColumn
            row <- use gCurrentRow
            collided <- gets gCollides
            when collided $ do
                mat <- use gCurrentMat
                (c : cs) <- use gMats
                gTableau . at (column, toRow row + 1) ?= mat
                gCurrentMat %= \(Colored _ _) -> Colored c Nothing
                gMats .= cs
                gCurrentRow .= 0.9
    draw :: Loop Globals
    draw = do
        lift clearScreen
        zoom _2 $ do
            zoom gTableau $ do
                sprites <- gets (map matMap . M.toList)
                lift $ drawSprites sprites
            column <- use gCurrentColumn
            row <- uses gCurrentRow toRow
            mat <- use gCurrentMat
            lift . drawSprite $ matMap ((column, row), mat)
        lift finishFrame

main :: IO ()
main = gemstoneMain getInitialState mainLoop
