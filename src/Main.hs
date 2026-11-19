module Main where

import Controller
import Model
import View
import System.Directory (doesFileExist)
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    exists <- doesFileExist highScoreFilePath
    if (exists)
        then do
            return ()
        else 
            writeHighScore 0
    hs <- readHighScore
    playIO FullScreen
              black            -- Background color
              60               -- Frames per second
              (initialState hs) -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function