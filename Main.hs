module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S

type Radius = Float
type Position = (Float, Float)

class Renderable a where
  render :: a -> Picture

-- Utility renderable definitions
instance Renderable a => Renderable [a] where
  render = pictures . fmap render

instance Renderable Picture where
  render = id

main :: IO ()
main = do 
  play window background fps initialState render handleKeys update
    where

      window :: Display
      window = InWindow "Pong" (width, height) (offset, offset)

      background :: Color
      background = black

fps, width, height, offset :: Int
fps = 60
width = 300
height = 300
offset = 100
paddleSpeed = 1
ballSpeed = 20

ballColor, paddleColor :: Color
ballColor = dark red
paddleColor = light (light blue)

newtype Ball = Ball (Float, Float, Float, Float)  -- ^ Pong ball location and velocity (x, y, vx, vy).
instance Renderable Ball where
  render (Ball (x, y, _, _)) = translate x y $ color ballColor $ circleSolid 10
newtype Paddle = Paddle (Color, Float, Float)   -- ^ Utility type to keep track of paddles (color, x, y).
                                                -- Only `y` is manipulated by the player, x and color decide which player they are.
instance Renderable Paddle where
  render (Paddle (c, x, y)) = translate x y $ pictures 
      [color c $ rectangleSolid 26 86, 
      color paddleColor $ rectangleSolid 20 80]

paddleUp :: Paddle -> Paddle
paddleUp (Paddle (c, x, y)) = Paddle (c, x, min 100 $ y + paddleSpeed)
paddleDown :: Paddle -> Paddle
paddleDown (Paddle (c, x, y)) = Paddle (c, x, max (-100) $ y - paddleSpeed)
ballPos :: Ball -> (Float, Float)
ballPos (Ball (x, y, _, _)) = (x, y)
ballVell :: Ball -> (Float, Float)
ballVell (Ball (_, _, vx, vy)) = (vx, vy)

-- | Data describing the state of the Pong game
data PongGame = Game
  { ball :: Ball                -- ^ Pong ball (x, y) location.
  , player1 :: Paddle           -- ^ Position of left's paddle height.
  , player2 :: Paddle           -- ^ Position of right's paddle height.
  , player1Score :: Int         -- ^ The score of player1.
  , player2Score :: Int         -- ^ The score of player2.
  , keysDown :: S.Set Key         -- ^ The current keys that are down (that we care about).
  , paused :: Bool              -- ^ Whether the game is paused or not.
  }

initialState :: PongGame
initialState = Game
  { ball = Ball (-10, 30, 2 * ballSpeed, -6 * ballSpeed)
  , player1 = Paddle (rose, 120, 40)
  , player2 = Paddle (orange, -120, 60)
  , player1Score = 0
  , player2Score = 0
  , keysDown = S.empty
  , paused = False
  }

-- | Respond to key events
handleKeys :: Event -> PongGame -> PongGame
-- 'r' resets the ball to the center
handleKeys (EventKey (Char 'r') Down _ _) game = restart game
-- 'p' and space toggles whether the game is paused
handleKeys (EventKey (Char 'p') Down _ _) game = game {paused = not $ paused game}
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game = game {paused = not $ paused game}
-- 'w' moves player2 up
handleKeys (EventKey (Char 'w') Down _ _) game = game {keysDown = S.insert (Char 'w') $ keysDown game}
handleKeys (EventKey (Char 'w') Up _ _) game = game {keysDown = S.delete (Char 'w') $ keysDown game}
-- 's' moves player2 down
handleKeys (EventKey (Char 's') Down _ _) game = game {keysDown = S.insert (Char 's') $ keysDown game}
handleKeys (EventKey (Char 's') Up _ _) game = game {keysDown = S.delete (Char 's') $ keysDown game}
-- 'up' moves player1 up
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game {keysDown = S.insert (SpecialKey KeyUp) $ keysDown game}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game {keysDown = S.delete (SpecialKey KeyUp) $ keysDown game}
-- 'down' moves player1 down
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game {keysDown = S.insert (SpecialKey KeyDown) $ keysDown game}
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game {keysDown = S.delete (SpecialKey KeyDown) $ keysDown game}
-- 'q' badly quits the game
handleKeys (EventKey (Char 'q') Down _ _) game = 
  let p1score = player1Score game
      p2score = player2Score game
  in  error $ if p1score > p2score then "Player 1 wins!" else if p2score > p1score then "Player 2 wins!" else "It was a tie"
-- Everything else is ignored
handleKeys _ game = game

instance Renderable PongGame where
  -- | Convert a game state into a picture
  render game = 
    pictures [ render $ ball game, walls
             , render $ player1 game
             , render $ player2 game
             , score
             ]
    where
      --  The bottom and top walls.
      wall :: Float -> Picture
      wall offset =
        translate 0 offset $
          color wallColor $
            rectangleSolid 270 10

      wallColor = greyN 0.5
      walls = pictures [wall 150, wall (-150)]

      -- Draw the score too
      score :: Picture
      score = translate (-20) 100 $ scale 0.2 0.2 $ color white $ text (show (player1Score game) ++ "-" ++ show (player2Score game))

restart :: PongGame -> PongGame
restart game = initialState { keysDown = keysDown game
                            , player1Score = player1Score game
                            , player2Score = player2Score game
                            , player1 = player1 game
                            , player2 = player2 game
                            , paused = True
                            }

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update seconds game = if paused game then game else 
  if player1Win game then restart game { player1Score = 1 + player1Score game } else
    if player2Win game then restart game { player2Score = 1 + player2Score game } else
      doKeyActions . paddleBounce . wallBounce . moveBall seconds $ game

doKeyActions :: PongGame -> PongGame
doKeyActions game = foldl keyActions game (keysDown game)

keyActions :: PongGame -> Key -> PongGame
keyActions game (SpecialKey KeyUp) = game {player1 = paddleUp $ player1 game}
keyActions game (SpecialKey KeyDown) = game {player1 = paddleDown $ player1 game}
keyActions game (Char 'w') = game {player2 = paddleUp $ player2 game}
keyActions game (Char 's') = game {player2 = paddleDown $ player2 game}
keyActions game _ = game

player1Win :: PongGame -> Bool
player1Win (Game { ball = Ball (x, _, _, _) } ) = -115 > x

player2Win :: PongGame -> Bool
player2Win (Game { ball = Ball (x, _, _, _) } ) = 115 < x

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ball = Ball (x', y', vx, vy) }
  where
    -- Old locations and velocities.
    Ball (x, y, vx, vy) = ball game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ball = Ball (x, y, vx', vy) }    -- TODO: Normalize & Randomize the speeds
  where
    -- The old velocities
    Ball (x, y, vx, vy) = ball game
    Paddle (_, _, right) = player1 game
    Paddle (_, _, left) = player2 game
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
wallBounce game = game { ball = Ball (x, y, vx, vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10
    Ball (x, y, vx, vy) = ball game

    vy' = if wallCollision (x, y) radius
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

