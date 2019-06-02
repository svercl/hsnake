{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Graphics.Gloss.Data.Point.Arithmetic as G
import qualified Graphics.Gloss.Interface.Pure.Game   as G
import           System.Random

import Control.Lens

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

data Scene
  = Playing
  | MainMenu
  | AteSelf

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
  { _positions :: [Position]
  , _direction :: Direction
  }

makeLenses ''Snake

mkSnake :: Position -> Snake
mkSnake position = Snake
  { _positions = [position]
  , _direction = GoingNowhere
  }

advanceSnake :: Snake -> Bool -> Snake
advanceSnake snake ateFood = snake & positions .~ newPositions
  where
    newHead = snakePosition snake G.+ fromDirection (snake ^. direction)
    oldPositions = snake ^. positions
    newPositions = newHead : if ateFood then oldPositions else init oldPositions

changeDirection :: Snake -> Direction -> Snake
changeDirection snake newDirection
  | eitherEqual GoingLeft GoingRight ||
    eitherEqual GoingUp GoingDown = snake
  | otherwise                     = snake & direction .~ newDirection
  where
    eitherEqual this that =
      (currentDirection == this && newDirection == that) ||
      (currentDirection == that && newDirection == this)
    currentDirection = snake ^. direction

snakePosition :: Snake -> Position
snakePosition snake = head $ snake ^. positions

data World
  = World
  { _snake                 :: Snake
  , _possibleFoodPositions :: [Position]
  , _currentFoodPosition   :: Int
  , _currentScene          :: Scene
  }

makeLenses ''World

switchTo :: World -> Scene -> World
switchTo world newScene = world & currentScene .~ newScene

foodPosition :: World -> Position
-- TODO: Should be possible to use ^@. this funky looking thing
foodPosition world = (world ^. possibleFoodPositions) !! (world ^. currentFoodPosition)

worldToPicture :: World -> G.Picture
worldToPicture w@(World _ _ _ currentScene) =
  case currentScene of
    Playing -> playingToPicture w
    MainMenu -> mainMenuToPicture
    AteSelf -> ateSelfToPicture

playingToPicture :: World -> G.Picture
playingToPicture world = G.pictures [snakePicture, foodPicture]
  where
    segmentPicture x y = G.translate (x * segmentSizeF) (y * segmentSizeF) $ G.rectangleSolid segmentSizeF segmentSizeF
    snakePicture = G.pictures $ map (\(x, y) -> G.color snakeColor $ segmentPicture x y) (world ^. (snake . positions))
    (foodX, foodY) = foodPosition world
    foodPicture = G.color foodColor $ segmentPicture foodX foodY
    segmentSizeF = fromIntegral segmentSize

mainMenuToPicture :: G.Picture
mainMenuToPicture = G.translate (-200) 0 $ G.scale 0.2 0.2 $ G.text "Welcome to Snake. Press SPACE to start."

ateSelfToPicture :: G.Picture
ateSelfToPicture = G.translate (-200) 0 $ G.scale 0.2 0.2 $ G.text "You ate yourself! Why would you do that??"

handleEvent :: G.Event -> World -> World
handleEvent evt world =
  case world ^. currentScene of
    Playing -> playingEvent evt world
    MainMenu -> mainMenuEvent evt world
    AteSelf -> ateSelfEvent evt world

playingEvent :: G.Event -> World -> World
playingEvent (G.EventKey key state _ _) world
  | keyDown $ G.Char 'w'              = switchDirection GoingUp
  | keyDown $ G.Char 's'              = switchDirection GoingDown
  | keyDown $ G.Char 'a'              = switchDirection GoingLeft
  | keyDown $ G.Char 'd'              = switchDirection GoingRight
  | keyDown $ G.Char 'q'              = world & currentFoodPosition %~ succ
  | keyDown $ G.SpecialKey G.KeySpace = switchDirection GoingNowhere
  | otherwise = world
  where
    keyDown what = key == what && state == G.Down
    switchDirection dir = world & snake .~ changeDirection (world ^. snake) dir
playingEvent _ w = w

mainMenuEvent :: G.Event -> World -> World
mainMenuEvent (G.EventKey key _ _ _) w
  | key == G.SpecialKey G.KeySpace = switchTo w Playing
  | otherwise = w
mainMenuEvent _ w = w

ateSelfEvent :: G.Event -> World -> World
ateSelfEvent (G.EventKey key _ _ _) w
  | key == G.SpecialKey G.KeySpace = switchTo w MainMenu
  | otherwise = w
ateSelfEvent _ w = w

handleTime :: Float -> World -> World
handleTime delta world =
  case world ^. currentScene of
    Playing -> playingTime delta world
    MainMenu -> mainMenuTime world
    AteSelf -> ateSelfTime world

playingTime :: Float -> World -> World
playingTime delta world =
  let
    foodAte = snakePosition (world ^. snake) == foodPosition world
    newFoodPosition = if foodAte then world ^. currentFoodPosition + 1 else world ^. currentFoodPosition
  in world { _snake = advanceSnake (world ^. snake) foodAte
           , _currentFoodPosition = newFoodPosition
           }

mainMenuTime :: World -> World
mainMenuTime w = w

ateSelfTime :: World -> World
ateSelfTime w = w

main :: IO ()
main = do
  gen <- getStdGen
  let
    maxWidth = (segmentsAcrossWidth `div` 2) - 1
    randomXs = randomRs (-maxWidth, maxWidth) gen
    maxHeight = (segmentsAcrossHeight `div` 2) - 1
    randomYs = randomRs (-maxHeight, maxHeight) gen
    foodPositions = zip (map fromIntegral randomXs) (map fromIntegral randomYs)
    initialWorld = World { _snake = mkSnake (0.0, 0.0)
                         , _possibleFoodPositions = foodPositions
                         , _currentFoodPosition = 0
                         , _currentScene = MainMenu
                         }
    initialDisplay = G.InWindow "Snake" (screenWidth, screenHeight) (0, 0)
  G.play initialDisplay G.white 10 initialWorld worldToPicture handleEvent handleTime
