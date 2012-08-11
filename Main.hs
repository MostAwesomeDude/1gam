{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.IORef
import qualified Data.Map as Map
import Text.JSON

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image

type ImageMap = Map.Map FilePath Surface

data GlobalData = GlobalData { _screen   :: Surface
                             , _images   :: ImageMap
                             , _quitFlag :: Bool }
    deriving (Show)

makeLenses ''GlobalData

type Loop = StateT GlobalData IO ()

jsonToMap :: JSObject e -> Map.Map String e
jsonToMap = Map.fromList . fromJSObject

data SpriteSheet = SpriteSheet { _ssFilePath :: FilePath }
    deriving (Show)

makeLenses ''SpriteSheet

emptySpriteSheet = SpriteSheet ""

instance JSON SpriteSheet where
    readJSON (JSObject o) = let
        m = jsonToMap o
        in Ok $ Prelude.flip execState emptySpriteSheet $ do
            case Map.lookup "meta" m of
                Just (JSObject o') -> do
                    let m' = jsonToMap o'
                    case Map.lookup "image" m' of
                        Just (JSString s) -> ssFilePath .= fromJSString s
                        Nothing -> return ()
                Nothing -> return ()

    showJSON _ = undefined

getScreen :: IO GlobalData
getScreen = do
    screen <- setVideoMode 640 480 32 [DoubleBuf, SWSurface]
    return $ GlobalData screen Map.empty False

loadImage :: FilePath -> ImageMap -> IO ImageMap
loadImage path m = do
    surface <- load path
    return $ Map.insert path surface m

loadSheet :: FilePath -> Loop
loadSheet path = do
    json <- lift $ readFile path
    let sheet = case decodeStrict json of
            Ok a -> a
            Error s -> emptySpriteSheet
    m <- use images
    m' <- lift $ loadImage (sheet ^. ssFilePath) m
    images .= m'

mainLoop :: Loop
mainLoop = loadImages >> loop
    where
    loop = do
        event <- lift pollEvent
        case event of
            NoEvent -> return ()
            KeyDown (Keysym SDLK_ESCAPE _ _) -> quitFlag .= True
            _ -> lift . putStrLn $ show event
        s <- use screen
        lift $ SDL.flip s
        q <- use quitFlag
        unless q $ loop
    loadImages = do
        loadSheet "heather.json"

actualMain :: IO ()
actualMain = do
    initial <- getScreen
    runStateT mainLoop initial
    return ()

main :: IO ()
main = withInit [InitEverything] actualMain
