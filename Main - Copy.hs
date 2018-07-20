module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.Pure.Game
import Data.List (delete)

type Radius = Float
type Position = (Float, Float)

main :: IO ()
main = do 
    play window background fps initialState render handleKeys update
    

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

ballColor, paddleColor :: Color
ballColor = dark red
paddleColor = light (light blue)

fps :: Int
fps = 60

-- | Data describing the state of the Pong game
data PongGame = Game
  { ballLoc :: Position         -- ^ Pong ball (x, y) location.
  , ballVel :: Position         -- ^ Pong ball (x, y) velocity.
  , player1 :: Float            -- ^ Position of left's paddle height.
  , player2 :: Float            -- ^ Position of right's paddle height.
  , player1Score :: Int         -- ^ The score of player1.
  , player2Score :: Int         -- ^ The score of player2.
  , keysDown :: [Key]           -- ^ The current keys that are down (that we care about).
  , paused :: Bool              -- ^ Whether the game is paused or not.
  } deriving (Show)

initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (20, -60)
  , player1 = 40
  , player2 = 60
  , player1Score = 0
  , player2Score = 0
  , keysDown = []
  , paused = False
  }

-- | Respond to key events
handleKeys :: Event -> PongGame -> PongGame
-- 'r' resets the ball to the center
handleKeys (EventKey (Char 'r') Down _ _) game = restart game
-- 'p' toggles whether the game is paused
handleKeys (EventKey (Char 'p') Down _ _) game = game {paused = not $ paused game}
-- 'w' moves player2 up
handleKeys (EventKey (Char 'w') Down _ _) game = game {keysDown = (Char 'w'):(keysDown game)}
handleKeys (EventKey (Char 'w') Up _ _) game = game {keysDown = delete (Char 'w') $ keysDown game}
-- 's' moves player2 down
handleKeys (EventKey (Char 's') Down _ _) game = game {keysDown = (Char 's'):(keysDown game)}
handleKeys (EventKey (Char 's') Up _ _) game = game {keysDown = delete (Char 's') $ keysDown game}
-- 'up' moves player1 up
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game {keysDown = (SpecialKey KeyUp):(keysDown game)}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game {keysDown = delete (SpecialKey KeyUp) $ keysDown game}
-- 'down' moves player1 down
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game {keysDown = (SpecialKey KeyDown):(keysDown game)}
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game {keysDown = delete (SpecialKey KeyDown) $ keysDown game}
-- 'q' badly quits the game
handleKeys (EventKey (Char 'q') Down _ _) game = 
  let p1score = player1Score game
      p2score = player2Score game
  in  error $ if p1score > p2score then "Player 1 wins!" else if p2score > p1score then "Player 2 wins!" else "It was a tie"
-- Everything else is ignored
handleKeys _ game = game

-- | Convert a game state into a picture
render :: PongGame  -- ^ The game state to render
       -> Picture   -- ^ A picture of the game state
render game = 
  pictures [ ball, walls
           , mkPaddle rose 120 $ player1 game
           , mkPaddle orange (-120) $ player2 game
           , score
           ]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)
    
    -- Draw the score too
    score :: Picture
    score = translate (-20) 100 $ scale 0.2 0.2 $ color white $ text (show (player1Score game) ++ "-" ++ show (player2Score game))

restart :: PongGame -> PongGame
restart game = initialState { keysDown = keysDown game
                            , player1Score = player1Score game
                            , player2Score = player2Score game
                            , player1 = player1 game
                            , player2 = player2 game}

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update seconds game = if paused game then game else 
  if player1Win game then restart game {player1Score = 1 + player1Score game} else
    if player2Win game then restart game {player2Score = 1 + player2Score game} else
      doKeyActions . paddleBounce . wallBounce . moveBall seconds $ game

doKeyActions :: PongGame -> PongGame
doKeyActions game = foldl keyActions game (keysDown game)

keyActions :: PongGame -> Key -> PongGame
keyActions game (SpecialKey KeyUp) = game {player1 = min 100 $ 1 + player1 game}
keyActions game (SpecialKey KeyDown) = game {player1 = max (-100) $ (-1) + player1 game}
keyActions game (Char 'w') = game {player2 = min 100 $ 1 + player2 game}
keyActions game (Char 's') = game {player2 = max (-100) $ (-1) + player2 game}
keyActions game _ = game

player1Win :: PongGame -> Bool
player1Win game = -115 > (fst $ ballLoc game)

player2Win :: PongGame -> Bool
player2Win game = 115 < (fst $ ballLoc game)

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }    -- TODO: Normalize & Randomize the speeds
  where
    -- The old velocities
    (vx, vy) = ballVel game
    (x, y) = ballLoc game
    right = player1 game
    left = player2 game
    -- [check lower bound, check upper bound, check distance]
    rightCollision = and [right - 36 < y, y < right + 36, 100 < x]
    leftCollision = and [left - 36 < y, y < left + 36, x < -100]
    vx' = if leftCollision      -- Check left collisions
          then
            abs vx
          else
            if rightCollision   -- Check right collisions
            then
              (-1) * abs vx
            else
              vx                -- Do nothing


-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius
          then
             -- Update the velocity.
             -vy
           else
             -- Do nothing. Return the old velocity.
             vy

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral width / 2 
    bottomCollision = y + radius >=  fromIntegral width / 2

