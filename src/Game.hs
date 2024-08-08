module Game where

-- Import SDL but make it qualified so we can refer to it as SDL.<function>
import qualified SDL
-- Some SDL functions are easier to use if we import them unqualified:
import SDL (($=))
import Control.Monad (unless)

-- | The central game state that contains every single bit of information about
-- the current state of the game.
data GameState = GameState
    { statePlayer :: Player
    , stateEnemies :: [Enemy]
    , stateBullets :: [Bullet]
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

    -- Return the initial game state
    pure $ GameState
        { statePlayer = Player
            { playerPosition = (0, 0)
            , playerHealth = 100
            , playerSpriteCache = surface
            }
        , stateEnemies = []
        , stateBullets = []
        }

-- | This is the main loop of our application. It polls for events, acts on them,
-- renders, and loops to repeat forever until the user wants to quit.
gameLoop :: SDL.Renderer -> GameState -> IO ()
gameLoop renderer gamestate = do
    -- Get all the events that have happened since the last frame
    events <- SDL.pollEvents

    -- Update the game state
    updatedGameState <- updateState gamestate

    -- Draw the game state to the screen
    drawState renderer updatedGameState

    -- Check if the user has requested to close the window and if so, exit the game.
    let userRequestedClose = any isWindowCloseEvent events
    unless userRequestedClose (gameLoop renderer updatedGameState)
  where
    -- | This function checks if the event given to us by SDL is a window close event,
    -- (i.e. the user has clicked the close button on the window).
    isWindowCloseEvent event =
        case SDL.eventPayload event of
            SDL.WindowClosedEvent _data -> True
            _ -> False

-- | Update the game state based on the events that have happened
updateState :: GameState -> IO GameState
updateState gamestate = do
    -- We *could* use the events argument to see keyboard/mouse input, but SDL
    -- provides us with a much nicer way to poll keyboard state!
    -- For mouse input, check out https://hackage.haskell.org/package/sdl2-2.5.5.0/docs/SDL-Input-Mouse.html#g:3
    isPressed <- SDL.getKeyboardState

    -- Let's update the player position based on the arrow keys
    let (playerPosX, playerPosY) = playerPosition (statePlayer gamestate)
    let newX
            | isPressed SDL.ScancodeLeft = playerPosX - 1
            | isPressed SDL.ScancodeRight = playerPosX + 1
            | otherwise = playerPosX
    let newY
            | isPressed SDL.ScancodeUp = playerPosY - 1
            | isPressed SDL.ScancodeDown = playerPosY + 1
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

    -- ====== Draw the player!
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
