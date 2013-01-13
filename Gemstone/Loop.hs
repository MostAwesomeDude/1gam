{-# LANGUAGE TemplateHaskell #-}
module Gemstone.Loop where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL

import Gemstone.GL
import Gemstone.Timers

-- | The type of loops.
type Loop a = StateT (Gems, a) IO ()

data Gems = Gems { _gScreen    :: Surface
                 , _gQuitFlag  :: Bool
                 , _gTimers    :: Timers }
    deriving (Show)

makeLenses ''Gems

gems :: Simple Lens (Gems, a) Gems
gems = _1

resizeScreen :: GLsizei -> GLsizei -> IO Surface
resizeScreen w h = let
    flags = [OpenGL, DoubleBuf, Resizable]
    in do
        screen <- setVideoMode (fromIntegral w) (fromIntegral h) 32 flags
        resizeViewport w h
        return screen

getInitialGems :: IO Gems
getInitialGems = do
    screen <- resizeScreen 1 1
    return $ Gems screen False makeTimers

handlePureCoreEvent :: Event -> Gems -> Gems
handlePureCoreEvent (KeyDown (Keysym SDLK_ESCAPE _ _)) = gQuitFlag .~ True
handlePureCoreEvent _ = id

handleCoreEvent :: Event -> StateT Gems IO ()
handleCoreEvent (VideoResize w h) =
    gScreen <~ lift (resizeScreen (fromIntegral w) (fromIntegral h))
handleCoreEvent event = modify $ handlePureCoreEvent event

handleEvents :: (Event -> StateT a IO ()) -> Loop a
handleEvents handler = do
    event <- lift pollEvent
    zoom _1 $ handleCoreEvent event
    zoom _2 $ handler event
    -- Continue until all events have been handled.
    when (event /= NoEvent) $ handleEvents handler
