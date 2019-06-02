{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
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

type Position = G.Point
type Snake = G.Path

_x :: Field1 s t a b => Lens s t a b
_x = _1

_y :: Field2 s t a b => Lens s t a b
_y = _2

data Direction
  = North
  | South
  | East
  | West
  | Nowhere

fromDirection :: Direction -> G.Vector
fromDirection dir =
  case dir of
    North   -> ( 0.0,  1.0)
    South   -> ( 0.0, -1.0)
    East    -> ( 1.0,  0.0)
    West    -> (-1.0,  0.0)
    Nowhere -> ( 0.0,  0.0)

data Developer
  = MoveFood

data Action
  = Direction Direction
  | Developer Developer

data Scene
  = Playing
  | MainMenu
  | AteSelf

data World
  = World
  { _snakePositions        :: Snake
  , _snakeDirection        :: Direction
  , _possibleFoodPositions :: [Position]
  , _currentFoodIndex      :: Int
  , _currentScene          :: Scene
  , _keyBinds              :: Map G.Key Action
  , _keys                  :: Set G.Key
  }

makeLenses ''World

mkWorld :: Position -> [Position] -> World
mkWorld playerPosition foodPositions =
  World [playerPosition] Nowhere foodPositions 0 MainMenu initialKeybinds Set.empty
  where initialKeybinds =
          Map.fromList [ (G.Char 'w', Direction North)
                       , (G.Char 's', Direction South)
                       , (G.Char 'a', Direction West)
                       , (G.Char 'd', Direction East)
                       , (G.SpecialKey G.KeySpace, Direction Nowhere)
                       , (G.Char 'q', Developer MoveFood)
                       ]

maybeChangeDirection :: Direction -> Direction -> Direction
maybeChangeDirection currentDirection newDirection =
  case (currentDirection, newDirection) of
    (West, East)   -> West
    (East, West)   -> East
    (North, South) -> North
    (South, North) -> South
    _ -> newDirection

-- TODO(bsvercl): This is the most imperative bullshit I've ever seen,
-- but with Gloss screen coordinates it's a little difficult.
-- I'm sure there's a better way; I just haven't found one quite yet.
-- NOTE: HERE BE DRAGONS
wrap :: Position -> Position
wrap (x, y) = ( if x >= halfSegX
                then -halfSegX
                else if x <= -halfSegX
                then halfSegX
                else x
              , if y >= halfSegY
                then -halfSegY
                else if y <= -halfSegY
                then halfSegY
                else y
              )
  where
    halfSegX = fromIntegral $ div segmentsAcrossWidth 2
    halfSegY = fromIntegral $ div segmentsAcrossHeight 2

advance :: Snake -> Direction -> Bool -> Snake
advance positions direction ateFood = newHead : newPositions
  where
    newHead = wrap $ head positions G.+ fromDirection direction
    newPositions = if ateFood then positions else init positions

-- Switches to specified Scene
switchTo :: World -> Scene -> World
switchTo world newScene = world & currentScene .~ newScene

foodPosition :: World -> Position
foodPosition world = (world ^. possibleFoodPositions) !! (world ^. currentFoodIndex)

worldToPicture :: World -> G.Picture
worldToPicture world =
  case world ^. currentScene of
    Playing  -> playingToPicture world
    MainMenu -> mainMenuToPicture
    AteSelf  -> ateSelfToPicture

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
    Playing  -> playingEvent evt world
    MainMenu -> mainMenuEvent evt world
    AteSelf  -> ateSelfEvent evt world

playingEvent :: G.Event -> World -> World
playingEvent (G.EventKey key state _ _) world
  | keyDown $ G.Char 'w'              = switchDirection North
  | keyDown $ G.Char 's'              = switchDirection South
  | keyDown $ G.Char 'a'              = switchDirection West
  | keyDown $ G.Char 'd'              = switchDirection East
  | keyDown $ G.Char 'q'              = world & currentFoodIndex %~ succ
  | keyDown $ G.SpecialKey G.KeySpace = switchDirection Nowhere
  | otherwise = world
  where
    keyDown what = key == what && state == G.Down
    switchDirection dir = world & snakeDirection %~ flip maybeChangeDirection dir
playingEvent _ w = w

-- he :: G.Event -> World -> World
-- he (G.EventKey key state _ _) world
--   | state == G.Down = world & keys %~ S.insert key
--   | state == G.Up = world & keys %~ S.delete key
--   | otherwise = world

-- keyPressed :: World -> G.Key -> Bool
-- keyPressed world key = S.member key (world ^. keys)

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
