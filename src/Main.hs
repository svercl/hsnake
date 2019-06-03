{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (fromMaybe)
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import qualified Graphics.Gloss.Data.Point.Arithmetic as G
import qualified Graphics.Gloss.Interface.Pure.Game   as G
import           System.Random                        (getStdGen, randomRs)

-- TODO(bsvercl): The drawing is not exactly correct for anything.
-- TODO(bsvercl): After a little refactoring, the food positions are not right, either.

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

center :: Position
center = ( fromIntegral $ div screenWidth 2
         , fromIntegral $ div screenHeight 2
         )

type Position = G.Point
type Size = (Int, Int)
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
  { _snakePositions        :: Snake            -- | Positions occupied by the snake
  , _initialSnakePosition  :: Position         -- | Used for resetting after eating self
  , _snakeDirection        :: Direction        -- | Direction of snake
  , _possibleFoodPositions :: [Position]       -- | Infinite list of food positions
  , _currentFoodIndex      :: Int              -- | We use this to step through the list
  , _currentScene          :: Scene
  , _keyBinds              :: Map G.Key Action
  , _keys                  :: Set G.Key        -- | Currently pressed keys
  }

makeLenses ''World

-- | Convenience function for creating a world
mkWorld :: Position -> [Position] -> World
mkWorld playerPosition foodPositions =
  World [playerPosition] playerPosition Nowhere foodPositions 0 MainMenu initialKeybinds Set.empty
  where initialKeybinds =
          Map.fromList [ (G.Char 'w', Direction North)
                       , (G.Char 's', Direction South)
                       , (G.Char 'a', Direction West)
                       , (G.Char 'd', Direction East)
                       , (G.SpecialKey G.KeySpace, Direction Nowhere)
                       , (G.Char 'q', Developer MoveFood)
                       ]

-- | Returns a new direction only when they are not opposites
maybeChangeDirection :: Direction -> Direction -> Maybe Direction
maybeChangeDirection currentDirection newDirection =
  case (currentDirection, newDirection) of
    (West, East)   -> Nothing
    (East, West)   -> Nothing
    (North, South) -> Nothing
    (South, North) -> Nothing
    _              -> Just newDirection

-- | Wraps a position around size
wrapAround :: Position -> Size -> Position
wrapAround (x, y) (w, h) = ( fromIntegral $ mod (floor x) w
                           , fromIntegral $ mod (floor y) h
                           )

-- | Wraps a position around the screen
wrap :: Position -> Position
wrap = flip wrapAround (segmentsAcrossWidth, segmentsAcrossHeight)

advance :: Snake -> Direction -> Bool -> Snake
advance positions direction ateFood = newHead : newPositions
  where
    newHead = wrap $ head positions G.+ fromDirection direction
    newPositions = if ateFood then positions else init positions

-- | Does snake intersect with itself?
didEatSelf :: Snake -> Bool
didEatSelf positions = head positions `elem` tail positions

-- | Switches to specified Scene
switchTo :: World -> Scene -> World
switchTo world newScene = world & currentScene .~ newScene

-- | The current food position
foodPosition :: World -> Position
foodPosition world = (world ^. possibleFoodPositions) !! (world ^. currentFoodIndex)

-- | Draws the world
worldToPicture :: World -> G.Picture
worldToPicture world =
  G.translate (negate $ fromIntegral $ div screenWidth  2)
              (negate $ fromIntegral $ div screenHeight 2) frame
  where
    frame =
      case world ^. currentScene of
        Playing  -> playingToPicture world
        MainMenu -> mainMenuToPicture
        AteSelf  -> ateSelfToPicture

-- | Draws game scene
playingToPicture :: World -> G.Picture
playingToPicture world = G.pictures [snakePicture, foodPicture]
  where
    segmentSizeF = fromIntegral segmentSize
    segmentPicture (x, y) = G.translate ((x * segmentSizeF) - (segmentSizeF / 2.0))
                                        ((y * segmentSizeF) - (segmentSizeF / 2.0))
                            $ G.rectangleSolid segmentSizeF segmentSizeF
    snakePicture = G.pictures $ map (G.color snakeColor . segmentPicture) (world ^. snakePositions)
    foodPicture = G.color foodColor $ segmentPicture (world ^. to foodPosition)

-- | Draws main menu scene
mainMenuToPicture :: G.Picture
mainMenuToPicture = G.translate (x - 250.0) y $ G.scale 0.2 0.2 $ G.text "Welcome to Snake. Press SPACE to start."
  where (x, y) = center

-- | Draws game over scene
ateSelfToPicture :: G.Picture
ateSelfToPicture = G.translate (x - 250.0) y $ G.scale 0.2 0.2 $ G.text "You ate yourself! Why would you do that??"
  where (x, y) = center

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
    switchDirection dir = world & snakeDirection %~ \current -> fromMaybe current (maybeChangeDirection current dir)
playingEvent _ w = w

-- he :: G.Event -> World -> World
-- he (G.EventKey key state _ _) world
--   | state == G.Down = world & keys %~ S.insert key
--   | state == G.Up = world & keys %~ S.delete key
--   | otherwise = world

-- keyPressed :: World -> G.Key -> Bool
-- keyPressed world key = S.member key (world ^. keys)

mainMenuEvent :: G.Event -> World -> World
mainMenuEvent (G.EventKey key G.Down _ _) w
  | key == G.SpecialKey G.KeySpace = switchTo w Playing
  | otherwise = w
mainMenuEvent _ w = w

ateSelfEvent :: G.Event -> World -> World
ateSelfEvent (G.EventKey key G.Down _ _) w
  | key == G.SpecialKey G.KeySpace = switchTo w MainMenu
  | otherwise = w
ateSelfEvent _ w = w

handleTime :: Float -> World -> World
handleTime delta world =
  case world ^. currentScene of
    Playing  -> playingTime delta world
    MainMenu -> mainMenuTime world
    AteSelf  -> ateSelfTime world

playingTime :: Float -> World -> World
playingTime _ world =
  let
    snake = world ^. snakePositions
    foodAte = head snake == world ^. to foodPosition
    foodIndex = world ^. currentFoodIndex
    newFoodIndex = if foodAte then succ foodIndex else foodIndex
    newSnakePositions = advance snake (world ^. snakeDirection) foodAte
  in if didEatSelf snake
     then snakePositions .~ [world ^. initialSnakePosition] $ switchTo world AteSelf
     else currentFoodIndex .~ newFoodIndex $ snakePositions .~ newSnakePositions $ world

mainMenuTime :: World -> World
mainMenuTime w = w

ateSelfTime :: World -> World
ateSelfTime w = w

main :: IO ()
main = do
  -- TODO(bsvercl): This is ugly, but it works.
  gen <- getStdGen
  let
    randomXs = randomRs (0, segmentsAcrossWidth) gen
    randomYs = randomRs (0, segmentsAcrossHeight) gen
    foodPositions = zip (map fromIntegral randomXs) (map fromIntegral randomYs)
    initialWorld = mkWorld center foodPositions
    initialDisplay = G.InWindow "Snake" (screenWidth, screenHeight) (0, 0)
  G.play initialDisplay G.white 10 initialWorld worldToPicture handleEvent handleTime
