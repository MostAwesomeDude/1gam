{-# LANGUAGE TemplateHaskell #-}
module Gemstone.Timers where

import Control.Lens
import Data.Word

import Gemstone.Maths

data Timers = Timers { _tTimestamp :: Word32
                     , _tDelta     :: Word32
                     , _tFps       :: Float }
    deriving (Show)

makeLenses ''Timers

makeTimers :: Timers
makeTimers = Timers 0 0 0

updateTimestamp :: Word32 -> Timers -> Timers
updateTimestamp w t = let
    delta = w - (t ^. tTimestamp)
    fps = 1000 / fromIntegral delta
    in tTimestamp .~ w $ tDelta .~ delta $ tFps %~ mma fps $ t
