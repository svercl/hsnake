module Main where

import           Data.Maybe                           (fromMaybe)
import           Debug.Trace
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

segmentSizeF :: Float
segmentSizeF = fromIntegral segmentSize

foodColor, snakeColor :: G.Color
foodColor = violet
snakeColor = orange

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
  | currentDirection == GoingLeft && newDirection == GoingRight = sn { direction = currentDirection }
  | currentDirection == GoingRight && newDirection == GoingLeft = sn { direction = currentDirection }
  | currentDirection == GoingUp && newDirection == GoingDown = sn { direction = currentDirection }
  | currentDirection == GoingDown && newDirection == GoingUp = sn { direction = currentDirection }
  | otherwise = sn { direction = newDirection }

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
    segmentPicture = G.rectangleSolid segmentSizeF segmentSizeF
    snakePicture = G.pictures $ map (\(x, y) -> G.translate (x * segmentSizeF)
                                                            (y * segmentSizeF)
                                              $ G.color snakeColor segmentPicture) snakePositions
    (foodX, foodY) = foodPosition w
    foodPicture = G.translate (foodX * segmentSizeF) (foodY * segmentSizeF) $ G.color foodColor segmentPicture

handleEvent :: G.Event -> World -> World
handleEvent (G.EventKey key state _ _) w@(World snake _ currentFoodPosition)
  | key == G.Char 'w' && state == G.Down = w { snake = changeDirection snake GoingUp }
  | key == G.Char 's' && state == G.Down = w { snake = changeDirection snake  GoingDown }
  | key == G.Char 'a' && state == G.Down = w { snake = changeDirection snake GoingLeft }
  | key == G.Char 'd' && state == G.Down = w { snake = changeDirection snake GoingRight }
  | key == G.Char 'q' && state == G.Down = w { currentFoodPosition = succ currentFoodPosition }
  | key == G.SpecialKey G.KeySpace && state == G.Down = w { snake = changeDirection snake GoingNowhere }
  | otherwise = w
handleEvent _ w = w

handleTime :: Float -> World -> World
handleTime delta w@(World snake possibleFoodPositions currentFoodPosition) =
  let
    foodAte = snakePosition snake == foodPosition w
  in w { snake = advanceSnake snake foodAte
       , currentFoodPosition = if foodAte then currentFoodPosition + 1 else currentFoodPosition
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
  G.play initialDisplay G.white 15 initialWorld worldToPicture handleEvent handleTime
