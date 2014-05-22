{-# LANGUAGE TemplateHaskell #-}
-- Copyright (C) 2014 Google Inc. All rights reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain
-- a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied. See the License for the specific language governing
-- permissions and limitations under the License.
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word

import Codec.Image.STB
import Data.Bitmap.OpenGL
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
                       , _gWords :: S.Set String
                       , _gCurrentColumn :: Int
                       , _gCurrentRow :: GLfloat
                       , _gCurrentMat :: Material
                       , _gMats :: [Material] }
    deriving (Show)

makeLenses ''Globals

loadTexture :: FilePath -> IO TextureObject
loadTexture path = do
    ei <- loadImage path
    let b = case ei of
            Left x  -> error $ "Error loading " ++ show path ++ ": " ++ show x
            Right x -> x
    makeSimpleBitmapTexture b

loadLetter :: Int -> IO TextureObject
loadLetter i = let
    num = if i < 10 then '0':show i else show i
    path = "FallingBee-assets/letters" ++ num ++ ".png"
    in loadTexture path

getInitialState :: IO Globals
getInitialState = do
    mats <- mapM loadLetter [0..25]
    ws <- readFile "four-letters.txt"
    let mats' = cycle $ map Textured mats
        s = S.fromList $ lines ws
    return $ Globals M.empty s 5 0.9 (Colored blue Nothing) mats'

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
    KeyDown (Keysym SDLK_LEFT _ _) -> gCurrentColumn -= 1
    KeyDown (Keysym SDLK_RIGHT _ _) -> gCurrentColumn += 1
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
                (m : ms) <- use gMats
                gTableau . at (column, toRow row + 1) ?= mat
                gCurrentMat .= m
                gMats .= ms
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
