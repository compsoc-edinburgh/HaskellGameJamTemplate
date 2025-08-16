module Game where

import qualified Raylib.Core as Raylib
import qualified Raylib.Core.Text as Raylib
import qualified Raylib.Core.Shapes as Raylib
import qualified Raylib.Core.Textures as Raylib
import qualified Raylib.Types as Raylib
import qualified Raylib.Util.Colors as Color

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
    { playerPosition :: (Double, Double)
    , playerHealth :: Int
    , playerSpriteCache :: Raylib.Texture
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

    -- We load the player texture from a PNG file. We *could* do this in the
    -- draw function when we need it (on demand), but it'll be laggy!!
    -- (Because accessing files is slow compared to how fast a game runs)
    texture2d <- Raylib.loadImage "assets/char0.png" >>= Raylib.loadTextureFromImage

    putStrLn "[info] Game initialisation complete!"

    -- Return the initial game state
    pure $ GameState
        { statePlayer = Player
            { playerPosition = (0, 0)
            , playerHealth = 100
            , playerSpriteCache = texture2d
            }
        , stateEnemies = []
        , stateBullets = []
        }

-- | Update the game state based on the events that have happened.
-- You may want to separate this into smaller functions, like updatePlayer,
-- updateEnemies, updateWorld, etc.
updateState :: GameState -> Double -> IO GameState
updateState gamestate deltaTime = do
    leftPressed <- Raylib.isKeyDown Raylib.KeyLeft
    rightPressed <- Raylib.isKeyDown Raylib.KeyRight
    upPressed <- Raylib.isKeyDown Raylib.KeyUp
    downPressed <- Raylib.isKeyDown Raylib.KeyDown

    -- Let's update the player position based on the arrow keys.
    -- Note that we're using the deltaTime to make the movement consistent
    -- on various framerates. It's like saying "move 500 pixels per second" and
    -- then multiplying that by the time since the last frame.
    let (playerPosX, playerPosY) = playerPosition (statePlayer gamestate)
    let newX
            | leftPressed = playerPosX - 500 * deltaTime
            | rightPressed = playerPosX + 500 * deltaTime
            | otherwise = playerPosX
    let newY
            | upPressed = playerPosY - 500 * deltaTime
            | downPressed = playerPosY + 500 * deltaTime
            | otherwise = playerPosY
    let newPosition = (newX, newY)

    -- Construct the new player state
    let newPlayer = (statePlayer gamestate) { playerPosition = newPosition }

    -- Return the new game state with the updated player
    pure gamestate { statePlayer = newPlayer }

-- | Draw the game state to the screen
drawState :: GameState -> IO ()
drawState gamestate = do
    Raylib.beginDrawing

    -- ====== Clear the screen!
    Raylib.clearBackground Color.white

    -- ====== Draw a rectangle (for no reason)! At 100,100 with a size of 100x200
    Raylib.drawRectangleLines 100 100 100 200 Color.red

    Raylib.drawText "Hello, world from raylib!" 30 40 20 Color.black

    -- ====== Draw the player sprite, we need to give it integer coordinates
    let (playerPosX, playerPosY) = playerPosition (statePlayer gamestate)
    Raylib.drawTexture (playerSpriteCache (statePlayer gamestate)) (round playerPosX) (round playerPosY) Color.white

    -- Alternatively, use drawTexturePro to control source size and dest size
    -- Below, we specify zooming 24px 24px source into 64px 64px destination
    -- Raylib.drawTexturePro (playerSpriteCache (statePlayer gamestate)) (Raylib.Rectangle 0 0 24 24) (Raylib.Rectangle (realToFrac playerPosX) (realToFrac playerPosY) 64 64) (Raylib.Vector2 0 0) 0 Color.white

    -- ====== Show the result to the screen!
    Raylib.endDrawing
