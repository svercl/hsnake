module Main where

import qualified Graphics.Gloss.Data.Point.Arithmetic as G
import qualified Graphics.Gloss.Interface.Pure.Game   as G
import           System.Random

screenWidth, screenHeight :: Int
screenWidth = 800
screenHeight = 600

segmentSize, segmentsAcrossWidth, segmentsAcrossHeight :: Int
segmentSize = 25
segmentsAcrossWidth = screenWidth `div` segmentSize
segmentsAcrossHeight = screenHeight `div` segmentSize

foodColor, snakeColor :: G.Color
foodColor = G.violet
snakeColor = G.orange

type Position = G.Point

data Direction
  = GoingLeft
  | GoingDown
  | GoingUp
  | GoingRight
  | GoingNowhere
  deriving (Eq)

fromDirection :: Direction -> G.Point
fromDirection dir =
  case dir of
    GoingLeft    -> (-1.0, 0.0)
    GoingDown    -> (0.0, -1.0)
    GoingUp      -> (0.0, 1.0)
    GoingRight   -> (1.0, 0.0)
    GoingNowhere -> (0.0, 0.0)

data Snake
  = Snake
  { positions :: [Position]
  , direction :: Direction
  }

mkSnake :: Position -> Snake
mkSnake position = Snake
  { positions = [position]
  , direction = GoingNowhere
  }

advanceSnake :: Snake -> Bool -> Snake
advanceSnake sn@(Snake positions direction) ateFood = sn { positions = newPositions }
  where
    newHead = snakePosition sn G.+ fromDirection direction
    newPositions = newHead : if ateFood then positions else init positions

changeDirection :: Snake -> Direction -> Snake
changeDirection sn@(Snake _ currentDirection) newDirection
  | eitherEqual GoingLeft GoingRight ||
    eitherEqual GoingUp GoingDown    = sn { direction = currentDirection }
  | otherwise                        = sn { direction = newDirection }
  where
    eitherEqual this that =
      (currentDirection == this && newDirection == that) ||
      (currentDirection == that && newDirection == this)

snakePosition :: Snake -> Position
snakePosition (Snake positions _) = head positions

data World
  = World
  { snake                 :: Snake
  , possibleFoodPositions :: [Position]
  , currentFoodPosition   :: Int
  }

foodPosition :: World -> Position
foodPosition (World _ possible current) = possible !! current

worldToPicture :: World -> G.Picture
worldToPicture w@(World (Snake snakePositions _) _ _) = G.pictures [snakePicture, foodPicture]
  where
    segmentPicture x y = G.translate (x * segmentSizeF) (y * segmentSizeF) $ G.rectangleSolid segmentSizeF segmentSizeF
    snakePicture = G.pictures $ map (\(x, y) -> G.color snakeColor $ segmentPicture x y) snakePositions
    (foodX, foodY) = foodPosition w
    foodPicture = G.color foodColor $ segmentPicture foodX foodY
    segmentSizeF = fromIntegral segmentSize

handleEvent :: G.Event -> World -> World
-- TODO: I swear this can be made much nicer
handleEvent (G.EventKey key state _ _) w@(World snake _ currentFoodPosition)
  | keyDown $ G.Char 'w'              = w { snake = changeDirection snake GoingUp }
  | keyDown $ G.Char 's'              = w { snake = changeDirection snake GoingDown }
  | keyDown $ G.Char 'a'              = w { snake = changeDirection snake GoingLeft }
  | keyDown $ G.Char 'd'              = w { snake = changeDirection snake GoingRight }
  | keyDown $ G.Char 'q'              = w { currentFoodPosition = succ currentFoodPosition }
  | keyDown $ G.SpecialKey G.KeySpace = w { snake = changeDirection snake GoingNowhere }
  | otherwise = w
  where
    keyDown what = key == what && state == G.Down
-- Do nothing
handleEvent _ w = w

handleTime :: Float -> World -> World
handleTime delta w@(World snake possibleFoodPositions currentFoodPosition) =
  let
    foodAte = snakePosition snake == foodPosition w
    newFoodPosition = if foodAte then currentFoodPosition + 1 else currentFoodPosition
  in w { snake = advanceSnake snake foodAte
       , currentFoodPosition = newFoodPosition
       }

main :: IO ()
main = do
  gen <- getStdGen
  let
    maxWidth = (segmentsAcrossWidth `div` 2) - 1
    randomXs = randomRs (-maxWidth, maxWidth) gen
    maxHeight = (segmentsAcrossHeight `div` 2) - 1
    randomYs = randomRs (-maxHeight, maxHeight) gen
    foodPositions = zip (map fromIntegral randomXs) (map fromIntegral randomYs)
    initialWorld = World { snake = mkSnake (0.0, 0.0)
                         , possibleFoodPositions = foodPositions
                         , currentFoodPosition = 0
                         }
    initialDisplay = G.InWindow "Snake" (screenWidth, screenHeight) (0, 0)
  G.play initialDisplay G.white 10 initialWorld worldToPicture handleEvent handleTime
