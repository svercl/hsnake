{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Graphics.Gloss.Data.Point.Arithmetic as G
import qualified Graphics.Gloss.Interface.Pure.Game   as G
import           System.Random (getStdGen, randomRs)

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

fromDirection :: Direction -> G.Point
fromDirection dir =
  case dir of
    GoingLeft    -> (-1.0, 0.0)
    GoingDown    -> (0.0, -1.0)
    GoingUp      -> (0.0, 1.0)
    GoingRight   -> (1.0, 0.0)
    GoingNowhere -> (0.0, 0.0)

data Developer
  = MoveFood

data Action
  = Direction Direction
  | Developer Developer

type Snake = [Position]

data World
  = World
  { _snake                 :: Snake
  , _snakeDirection        :: Direction
  , _possibleFoodPositions :: [Position]
  , _currentFoodIndex      :: Int
  , _currentScene          :: Scene
  , _keyBinds              :: M.Map G.Key Action
  , _pressedKeys           :: [G.Key] -- TODO(bsvercl): Use something more efficient.
  }

makeLenses ''World

mkWorld :: Position -> [Position] -> World
mkWorld playerPosition foodPositions =
  World [playerPosition] GoingNowhere foodPositions 0 MainMenu initialKeybinds []
  where initialKeybinds = M.fromList [ (G.Char 'w', Direction GoingUp)
                                     , (G.Char 's', Direction GoingDown)
                                     , (G.Char 'a', Direction GoingLeft)
                                     , (G.Char 'd', Direction GoingRight)
                                     , (G.Char 'q', Developer MoveFood)
                                     ]

playerPosition :: World -> Position
playerPosition world = head $ world ^. snake

maybeChangeDirection :: Direction -> Direction -> Direction
maybeChangeDirection currentDirection newDirection =
  case (currentDirection, newDirection) of
    (GoingLeft, GoingRight) -> GoingLeft
    (GoingRight, GoingLeft) -> GoingRight
    (GoingUp, GoingDown)    -> GoingUp
    (GoingDown, GoingDown)  -> GoingDown
    _ -> newDirection

advance :: World -> Bool -> World
advance world ateFood = world & snake .~ newSnake
  where
    newHead = playerPosition world G.+ fromDirection (world ^. snakeDirection)
    positions = world ^. snake
    newSnake = newHead : if ateFood then positions else init positions

-- Switches to specified Scene
switchTo :: World -> Scene -> World
switchTo world newScene = world & currentScene .~ newScene

foodPosition :: World -> Position
-- TODO: Should be possible to use ^@. this funky looking thing
foodPosition world = (world ^. possibleFoodPositions) !! (world ^. currentFoodIndex)

worldToPicture :: World -> G.Picture
worldToPicture world =
  case world ^. currentScene of
    Playing -> playingToPicture world
    MainMenu -> mainMenuToPicture
    AteSelf -> ateSelfToPicture

playingToPicture :: World -> G.Picture
playingToPicture world = G.pictures [snakePicture, foodPicture]
  where
    segmentPicture x y = G.translate (x * segmentSizeF) (y * segmentSizeF) $ G.rectangleSolid segmentSizeF segmentSizeF
    snakePicture = G.pictures $ map (\(x, y) -> G.color snakeColor $ segmentPicture x y) (world ^. snake)
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
  | keyDown $ G.Char 'q'              = world & currentFoodIndex %~ succ
  | keyDown $ G.SpecialKey G.KeySpace = switchDirection GoingNowhere
  | otherwise = world
  where
    keyDown what = key == what && state == G.Down
    switchDirection dir = world & snakeDirection %~ flip maybeChangeDirection dir --world & snakeDirection .~ maybeChangeDirection (world ^. snakeDirection) dir
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
    foodAte = world ^. to playerPosition == world ^. to foodPosition
    -- I wish we were inside of a Monad
    foodIndex' = if foodAte then world ^. currentFoodIndex + 1 else world ^. currentFoodIndex
    world' = advance world foodAte
    world'' = world' & currentFoodIndex .~ foodIndex'
  in world''

mainMenuTime :: World -> World
mainMenuTime w = w

ateSelfTime :: World -> World
ateSelfTime w = w

main :: IO ()
main = do
  -- TODO(bsvercl): This is ugly, but it works.
  gen <- getStdGen
  let
    maxWidth = (segmentsAcrossWidth `div` 2) - 1
    randomXs = randomRs (-maxWidth, maxWidth) gen
    maxHeight = (segmentsAcrossHeight `div` 2) - 1
    randomYs = randomRs (-maxHeight, maxHeight) gen
    foodPositions = zip (map fromIntegral randomXs) (map fromIntegral randomYs)
    initialWorld = mkWorld (0.0, 0.0) foodPositions
    initialDisplay = G.InWindow "Snake" (screenWidth, screenHeight) (0, 0)
  G.play initialDisplay G.white 10 initialWorld worldToPicture handleEvent handleTime
