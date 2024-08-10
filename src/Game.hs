module Game where

-- Import SDL but make it qualified so we can refer to it as SDL.<function>
import qualified SDL
import qualified SDL.Raw.Timer as SDL (getTicks)
-- Some SDL functions are easier to use if we import them unqualified:
import SDL (($=))
import Control.Monad (unless)

-- | The central game state that contains every single bit of information about
-- the current state of the game.
data GameState = GameState
    { stateInternal :: InternalState
    , statePlayer :: Player
    , stateEnemies :: [Enemy]
    , stateBullets :: [Bullet]
    }

data InternalState = InternalState
    { internalNextGameTick :: Double
    -- ^ The time at which the next game step should occur.
    , internalLastGameTick :: Double
    -- ^ The time at which the last game step occurred, to calculate FPS.
    , internalLastFiveFrameTimes :: [Double]
    -- ^ The last five frame times, to smoothen the FPS calculation.
    }

-- | These are arbitrary data types that represent the player, enemies, and bullets.
-- Feel free to edit or add to these as you see fit!
data Player = Player
    { playerPosition :: (Int, Int)
    , playerHealth :: Int
    , playerSpriteCache :: SDL.Surface
    }

-- | See above, this is a template.
data Enemy = Enemy
    { enemyPosition :: (Int, Int)
    , enemyHealth :: Int
    }


-- | See above, this is a template.
data Bullet = Bullet
    { bulletPosition :: (Int, Int)
    , bulletDirection :: (Int, Int)
    }

-- | This function runs when the game starts. Initialise the game state, perform
-- any startup calculations or actions here.
-- To see where this is called, check out Main.hs.
init :: IO GameState
init = do
    putStrLn "[INFO] Game initialisation! Loading assets..."

    -- We load the player texture from a BMP file. We *could* do this in the
    -- draw function when we need it (on demand), but it'll be laggy!!
    -- (Because accessing files is slow compared to how fast a game runs)
    surface <- SDL.loadBMP "assets/char0.bmp"

    putStrLn "[INFO] Game initialisation complete!"

    -- Get the current milliseconds since SDL was initialised as a Word32
    ticks <- SDL.getTicks

    -- Return the initial game state
    pure $ GameState
        { stateInternal = InternalState
            { internalNextGameTick = fromIntegral ticks
            , internalLastGameTick = 0
            , internalLastFiveFrameTimes = []
            }
        , statePlayer = Player
            { playerPosition = (0, 0)
            , playerHealth = 100
            , playerSpriteCache = surface
            }
        , stateEnemies = []
        , stateBullets = []
        }

-- | Target frames per second for the game loop. We use a fixed timestep game
-- loop in this template to make it easier to reason about the game's state.
targetFPS :: Int
targetFPS = 60

-- | This is the main loop of our application. In the most basic sense, it is a
-- loop that continuously polls for events, acts on them, renders, and repeats
-- until the user wants to quit.
-- There is added functionality to make sure the game runs at a consistent speed
-- (targetFPS) so that the game speed is not tied to the speed of the computer
-- running it. This is called a "fixed timestep" game loop. It is implemented by
-- delaying the thread when we are faster than the target FPS.
gameLoop :: SDL.Renderer -> GameState -> IO ()
gameLoop renderer gamestate = do
    -- Get the current time since game start in milliseconds
    now <- fromIntegral <$> SDL.getTicks

    -- When should we render the next frame?
    let nextFrameTick = internalNextGameTick (stateInternal gamestate)
    if nextFrameTick > now
        -- If we're ahead of schedule for target FPS, wait until the next frame
        then do
            SDL.delay (fromIntegral (round (nextFrameTick - now) :: Int))
            gameLoop renderer gamestate
        else do
            -- Get all the events that have happened since the last frame
            events <- SDL.pollEvents

            -- Update the game state
            updatedGameState <- updateState gamestate

            -- Draw the game state to the screen
            drawState renderer updatedGameState

            -- Check if the user has requested to close the window and only loop
            -- if they haven't!
            let userRequestedClose = any isWindowCloseEvent events
            unless userRequestedClose $ do
                -- Print the current frames per second to the console for debugging
                printFPS (stateInternal gamestate)

                -- Continue the game loop with the updated game state, plus the
                -- scheduled time of the next frame and the current time.
                let lastFrameTick = internalLastGameTick (stateInternal gamestate)
                gameLoop renderer updatedGameState
                    { stateInternal = InternalState
                        { internalNextGameTick = nextFrameTick + (1000 / fromIntegral targetFPS)
                        , internalLastGameTick = now
                        , internalLastFiveFrameTimes = (now - lastFrameTick) : take 4 (internalLastFiveFrameTimes (stateInternal gamestate))
                        }
                    }
  where
    -- | This function checks if the event given to us by SDL is a window close event,
    -- (i.e. the user has clicked the close button on the window).
    isWindowCloseEvent event =
        case SDL.eventPayload event of
            SDL.WindowClosedEvent _data -> True
            _ -> False

-- | This function prints the current frames per second to the console.
printFPS :: InternalState -> IO ()
printFPS internalState = do
    -- Calculate the average frame time over the last five frame times
    let lastFiveFrameTimes = internalLastFiveFrameTimes internalState
    let averageFrameTime = sum lastFiveFrameTimes / fromIntegral (max 1 (length lastFiveFrameTimes))
    let fps = 1000 / (max 1 averageFrameTime)
    -- Print the FPS and a newline to the console, overwriting the last two lines
    putStr $ "\x1b[1A\x1b[2K[INFO] FPS: " ++ show (round fps :: Int) ++ "\n"

-- | Update the game state based on the events that have happened
updateState :: GameState -> IO GameState
updateState gamestate = do
    -- SDL provides us with a nice way to check current keyboard press state!
    -- It's a function that accepts a Scancode (i.e a key) and returns a Bool.
    -- For mouse input, check out https://hackage.haskell.org/package/sdl2-2.5.5.0/docs/SDL-Input-Mouse.html#g:3
    isPressed <- SDL.getKeyboardState

    -- Let's update the player position based on the arrow keys
    let (playerPosX, playerPosY) = playerPosition (statePlayer gamestate)
    let newX
            | isPressed SDL.ScancodeLeft = playerPosX - 5
            | isPressed SDL.ScancodeRight = playerPosX + 5
            | otherwise = playerPosX
    let newY
            | isPressed SDL.ScancodeUp = playerPosY - 5
            | isPressed SDL.ScancodeDown = playerPosY + 5
            | otherwise = playerPosY
    let newPosition = (newX, newY)

    -- Construct the new player state
    let newPlayer = (statePlayer gamestate) { playerPosition = newPosition }

    -- Return the new game state with the updated player
    pure gamestate { statePlayer = newPlayer }

-- | Draw the game state to the screen
drawState :: SDL.Renderer -> GameState -> IO ()
drawState renderer gamestate = do
    -- Set the draw color for the screen, defined as a vector of RGB + Alpha.
    SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255

    -- Clear/Fill the buffer with the draw color. If we don't do this, we'll see
    -- the previous frame!
    -- Also, the buffer is not the screen, it's a "hidden" buffer that we draw to
    -- and then present to the screen later so everything appears at once.
    SDL.clear renderer

    -- Set the draw color again, this time red for a rectangle!
    SDL.rendererDrawColor renderer $= SDL.V4 255 0 0 255

    -- ====== Draw a rectangle! At 100,100 with a size of 100x200
    SDL.drawRect renderer (Just (SDL.Rectangle (SDL.P (SDL.V2 100 100)) (SDL.V2 100 200)))

    -- ====== Draw the player! Maybe a helper function would be nice here?
    -- First, convert the SDL Surface to an SDL Texture specific to this renderer buffer.
    -- Under the hood, this is copying the image data to the GPU in a temporary buffer!
    texture <- SDL.createTextureFromSurface renderer (playerSpriteCache (statePlayer gamestate))
    -- Determine where to render the player! SDL wants a Vector (V2) of CInts,
    -- so we use fromIntegral to convert from the Ints in our game state.
    let playerPos = playerPosition (statePlayer gamestate)
    let playerPosCInt = SDL.V2 (fromIntegral (fst playerPos)) (fromIntegral (snd playerPos))
    -- We can choose what size to render the sprite at, it'll stretch to fit.
    let renderSize = SDL.V2 64 64
    -- Copy the sprite to the final renderer buffer at the specified location
    SDL.copy renderer texture Nothing (Just (SDL.Rectangle (SDL.P playerPosCInt) renderSize))

    -- Present the renderer "buffer" to the window! This frees up the buffer
    -- for the next frame to be drawn.
    SDL.present renderer
