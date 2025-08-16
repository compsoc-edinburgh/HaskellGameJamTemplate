module Main where

import qualified Raylib.Core as Raylib
import qualified Raylib.Types as Raylib
import Raylib.Util (whileWindowOpen_)

import qualified Game

-- This is the starting point of our application. It initializes a Raylib window,
-- and enters the application loop.
main :: IO ()
main = do
    -- Turn on vsync, otherwise we'll see screen tearing when monitor refresh
    -- rates don't match the game speed
    Raylib.setConfigFlags [Raylib.VsyncHint]
    res <- Raylib.initWindow 800 480 "raylib [haskell] example - window"
    state <- Game.init
    -- Tell raylib to run the below loop at approx. 60 times per second
    Raylib.setTargetFPS 60

    (flip whileWindowOpen_) state $ \currentState -> do
        -- Calculate time in seconds from last update
        deltaTime <- Raylib.getFrameTime
        -- Update the game state
        newState <- Game.updateState currentState (realToFrac deltaTime)
        -- Draw the new game state
        Game.drawState newState
        -- Return the new game state for the next loop
        pure newState

    Raylib.closeWindow (Just res)
