module Game where

-- Import SDL but make it qualified so we can refer to it as SDL.<function>
import qualified SDL
import qualified SDL.Raw.Timer as SDL (getPerformanceFrequency, getPerformanceCounter)
-- Some SDL functions are easier to use if we import them unqualified:
import SDL (($=))
import Control.Monad (unless)

-- | The central game state that contains every single bit of information about
-- the current state of the game.
data GameState = GameState
    { stateInternalLastCounter :: Double
    -- ^ The SDL performance counter at the last frame.
    , statePlayer :: Player
    , stateEnemies :: [Enemy]
    , stateBullets :: [Bullet]
    }

-- | These are arbitrary data types that represent the player, enemies, and bullets.
-- Feel free to edit or add to these as you see fit!
data Player = Player
    { playerPosition :: (Double, Double)
    , playerHealth :: Int
    , playerSpriteCache :: SDL.Surface
    }

-- | See above, this is a template.
data Enemy = Enemy
    { enemyPosition :: (Double, Double)
    , enemyHealth :: Int
    }


-- | See above, this is a template.
data Bullet = Bullet
    { bulletPosition :: (Double, Double)
    , bulletDirection :: (Double, Double)
    }

-- | This function runs when the game starts. Initialise the game state, perform
-- any startup calculations or actions here.
-- To see where this is called, check out Main.hs.
init :: IO GameState
init = do
    putStrLn "[info] Game initialisation! Loading assets..."

    -- We load the player texture from a BMP file. We *could* do this in the
    -- draw function when we need it (on demand), but it'll be laggy!!
    -- (Because accessing files is slow compared to how fast a game runs)
    surface <- SDL.loadBMP "assets/char0.bmp"

    putStrLn "[info] Game initialisation complete!"

    -- Get the unit-less counter since SDL was initialised as a Word32
    counter <- SDL.getPerformanceCounter

    -- Return the initial game state
    pure $ GameState
        { stateInternalLastCounter = fromIntegral counter
        , statePlayer = Player
            { playerPosition = (0, 0)
            , playerHealth = 100
            , playerSpriteCache = surface
            }
        , stateEnemies = []
        , stateBullets = []
        }

-- | This is the main loop of our application. In the most basic sense, it is a
-- loop that continuously polls for events, acts on them, renders, and repeats
-- until the user wants to quit.
--
-- To save CPU cycles, this template has enabled VSync (in Main.hs; You can
-- disable VSync by removing the renderType part), which means the game will
-- try to run at the refresh rate of the monitor (e.g. 50Hz, 60Hz, 144Hz). SDL
-- will automatically handle this if enabled. If you disable VSync, the game
-- will run as fast as it can, which is usually undesirable because it will use
-- 100% of the CPU when it might not need to.
--
-- Obviously, because not all monitors are the same, the game will run at
-- different speeds on different devices. Even if you disable VSync, not all
-- CPUs are the same so the game will still run at different speeds on various
-- devices. This is called "variable frame rate". To make sure that the game
-- logic (e.g. player movement) is still the same speed on all devices, we use
-- a concept called "delta time". (Search for "delta time game loop" on Google!)
gameLoop :: SDL.Renderer -> GameState -> IO ()
gameLoop renderer gamestate = do
    -- Get an estimate of how quickly the SDL counter is increasing
    freq <- fromIntegral <$> SDL.getPerformanceFrequency

    -- Get the value of the SDL counter right now
    now <- fromIntegral <$> SDL.getPerformanceCounter

    -- Calculate the deltaTime (fractional seconds since the last frame) using
    -- the above information. This value needs to be a Double to keep the
    -- 64-bit presicion of the SDL counter.
    let deltaTime = (now - stateInternalLastCounter gamestate) / freq

    -- Get all the events that have happened since the last frame
    events <- SDL.pollEvents

    -- Update the game state according to our own logic
    updatedGameState <- updateState gamestate deltaTime

    -- Draw the game state to the screen
    drawState renderer updatedGameState

    -- Check if the user has requested to close the window. Otherwise loop!
    let userRequestedClose = any isWindowCloseEvent events
    unless userRequestedClose $ do
        -- Continue the game loop with the updated game state from our logic,
        -- plus the new SDL counter value.
        gameLoop renderer updatedGameState
            { stateInternalLastCounter = now
            }

  where
    -- | This function checks if the event given to us by SDL indicates that the
    -- game should end. E.g. CTRL-C, clicking the close button, CMD-Q, etc.
    isWindowCloseEvent event =
        case SDL.eventPayload event of
            SDL.WindowClosedEvent _data -> True
            SDL.QuitEvent -> True
            _ -> False

-- | Update the game state based on the events that have happened
updateState :: GameState -> Double -> IO GameState
updateState gamestate deltaTime = do
    -- SDL provides us with a nice way to check current keyboard press state!
    -- It's a function that accepts a Scancode (i.e a key) and returns a Bool.
    -- For mouse input, check out https://hackage.haskell.org/package/sdl2-2.5.5.0/docs/SDL-Input-Mouse.html#g:3
    isPressed <- SDL.getKeyboardState

    -- Let's update the player position based on the arrow keys.
    -- Note that we're using the deltaTime to make the movement consistent
    -- on various framerates. It's like saying "move 500 pixels per second" and
    -- then multiplying that by the time since the last frame.
    let (playerPosX, playerPosY) = playerPosition (statePlayer gamestate)
    let newX
            | isPressed SDL.ScancodeLeft = playerPosX - 500 * deltaTime
            | isPressed SDL.ScancodeRight = playerPosX + 500 * deltaTime
            | otherwise = playerPosX
    let newY
            | isPressed SDL.ScancodeUp = playerPosY - 500 * deltaTime
            | isPressed SDL.ScancodeDown = playerPosY + 500 * deltaTime
            | otherwise = playerPosY
    let newPosition = (newX, newY)

    -- Construct the new player state
    let newPlayer = (statePlayer gamestate) { playerPosition = newPosition }

    -- Return the new game state with the updated player
    pure gamestate { statePlayer = newPlayer }

-- | Draw the game state to the screen
drawState :: SDL.Renderer -> GameState -> IO ()
drawState renderer gamestate = do
    -- ====== Clear the screen!
    -- Set the draw color for the screen, defined as a vector of RGB + Alpha.
    SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255

    -- Clear/Fill the buffer with the draw color. If we don't do this, we'll see
    -- garbage! Try removing it and see for yourself if you're curious :)
    -- Also, the "buffer" is not the screen, it's a hidden place that we draw to
    -- and then present to the screen later so everything appears at once.
    SDL.clear renderer

    -- ====== Draw a rectangle! At 100,100 with a size of 100x200
    -- Set the draw color again, this time red for a rectangle!
    SDL.rendererDrawColor renderer $= SDL.V4 255 0 0 255
    SDL.drawRect renderer (Just (SDL.Rectangle (SDL.P (SDL.V2 100 100)) (SDL.V2 100 200)))

    -- ====== Draw the player! Maybe a helper function would be nice here?
    -- First, convert the SDL Surface to an SDL Texture specific to this renderer buffer.
    -- Under the hood, this is copying the image data to the GPU in a temporary buffer!
    texture <- SDL.createTextureFromSurface renderer (playerSpriteCache (statePlayer gamestate))

    -- Determine where to render the player! SDL wants a Vector (V2) of CFloat,
    -- so we use realToFrac to convert our Double player position to CFloat.
    let (playerPosX, playerPosY) = playerPosition (statePlayer gamestate)
    let playerPosCFloat = SDL.V2 (realToFrac playerPosX) (realToFrac playerPosY)

    -- We can choose what size to render the sprite at, it'll stretch to fit.
    let renderSize = SDL.V2 64 64

    -- Copy the sprite to the final renderer buffer at the specified location.
    -- (SDL.copyF is a variant of SDL.copy that allows floating point positions)
    SDL.copyF renderer texture Nothing (Just (SDL.Rectangle (SDL.P playerPosCFloat) renderSize))

    -- ====== Show the result to the screen!
    -- Present the renderer "buffer" to the window! This frees up the buffer
    -- for the next frame to be drawn.
    SDL.present renderer
