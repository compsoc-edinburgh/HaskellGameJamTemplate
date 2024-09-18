module Main where

import qualified SDL
import qualified Data.Text as Text

import Game

-- This is the starting point of our application. It initializes SDL2 which is
-- the library we will use to create a window and render graphics.
-- It then enters the application loop.
main :: IO ()
main = do
    putStrLn "[info] Starting SDL2, the window and graphics library"
    SDL.initializeAll

    putStrLn "[info] Creating a window"
    window <- SDL.createWindow (Text.pack "My Game Window") SDL.defaultWindow
    putStrLn "[info] Creating a renderer"
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }

    putStrLn "[info] Initialising the game"
    gameState <- Game.init

    putStrLn "[info] Starting the app loop"
    Game.gameLoop renderer gameState

    putStrLn "[info] Exiting"
    SDL.destroyWindow window
