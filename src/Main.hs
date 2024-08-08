module Main where

import qualified SDL
import qualified Data.Text as Text

import Game

-- This is the starting point of our application. It initializes SDL2 which is
-- the library we will use to create a window and render graphics.
-- It then enters the application loop.
main :: IO ()
main = do
    putStrLn "[INFO] Starting SDL2, the window and graphics library"
    SDL.initializeAll

    putStrLn "[INFO] Creating a window"
    window <- SDL.createWindow (Text.pack "My Game Window") SDL.defaultWindow
    putStrLn "[INFO] Creating a renderer"
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    putStrLn "[INFO] Initialising the game"
    gameState <- Game.init

    putStrLn "[INFO] Starting the app loop"
    Game.gameLoop renderer gameState

    putStrLn "[INFO] Exiting"
    SDL.destroyWindow window
