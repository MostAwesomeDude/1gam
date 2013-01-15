module Gemstone.Main where

import Control.Monad
import Control.Monad.Trans.State
import Graphics.UI.SDL as SDL

import Gemstone.GL
import Gemstone.Loop

gemstoneMain :: a -> Loop a -> IO ()
gemstoneMain globals loop = withInit [InitEverything] $ do
    initial <- getInitialGems
    checkExtensions
    void $ runStateT loop (initial, globals)
