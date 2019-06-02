{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
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

_x :: Field1 s t a b => Lens s t a b
_x = _1

_y :: Field2 s t a b => Lens s t a b
_y = _2

data Direction
  = GoingLeft
  | GoingDown
  | GoingUp
  | GoingRight
  | GoingNowhere

fromDirection :: Direction -> G.Vector
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

type Snake = G.Path

data World
  = World
  { _snakePositions        :: Snake
  , _snakeDirection        :: Direction
  , _possibleFoodPositions :: [Position]
  , _currentFoodIndex      :: Int
  , _currentScene          :: Scene
  , _keyBinds              :: M.Map G.Key Action
  , _keys                  :: Set G.Key
  }

makeLenses ''World

mkWorld :: Position -> [Position] -> World
mkWorld playerPosition foodPositions =
  World [playerPosition] GoingNowhere foodPositions 0 MainMenu initialKeybinds S.empty
  where initialKeybinds =
          M.fromList [ (G.Char 'w', Direction GoingUp)
                     , (G.Char 's', Direction GoingDown)
                     , (G.Char 'a', Direction GoingLeft)
                     , (G.Char 'd', Direction GoingRight)
                     , (G.Char 'q', Developer MoveFood)
                     ]

maybeChangeDirection :: Direction -> Direction -> Direction
maybeChangeDirection currentDirection newDirection =
  case (currentDirection, newDirection) of
    (GoingLeft, GoingRight) -> GoingLeft
    (GoingRight, GoingLeft) -> GoingRight
    (GoingUp, GoingDown)    -> GoingUp
    (GoingDown, GoingDown)  -> GoingDown
    _ -> newDirection

advance :: Snake -> Direction -> Bool -> Snake
advance positions direction ateFood = newHead : newPositions
  where newHead = head positions G.+ fromDirection direction
        newPositions = if ateFood then positions else init positions

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
    snakePicture = G.pictures $ map (\(x, y) -> G.color snakeColor $ segmentPicture x y) (world ^. snakePositions)
    foodPos = world & foodPosition
    foodPicture = G.color foodColor $ segmentPicture (foodPos ^. _x) (foodPos ^. _y)
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
    switchDirection dir = world & snakeDirection %~ flip maybeChangeDirection dir
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
    foodAte = head (world ^. snakePositions) == world ^. to foodPosition
    newFoodIndex = if foodAte then world ^. currentFoodIndex + 1 else world ^. currentFoodIndex
    newSnakePositions = advance (world ^. snakePositions) (world ^. snakeDirection) foodAte
  in currentFoodIndex .~ newFoodIndex $ snakePositions .~ newSnakePositions $ world

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
