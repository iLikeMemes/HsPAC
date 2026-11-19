{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where
import Graphics.Gloss
import Data.List
import Data.Maybe
import qualified Data.Vector
import GHC.Float

{- Small city to improve sanity:
                                                                  +
                                                               ___|___
                                                              /  ___  \
                                                             /  / | \  \
                                               _______      |  |  +--|  |
                                              /~  ~~  \     |   \___/   |
       _  /\       _  ________________       /  ~~~  ~ \    |           |
      | |/  \     | |/       __       \     |~~    ~~  ~|   |           |
      | /    \    | /       /  \       \    |   ~~~ ~   |   |  /\   /\  |
      |/      \   |/        \__/        \    \~~  ~~ ~~/    | |  | |  | |
      /________\  /______________________\    \_~~  ~_/     | |__| |__| |______________
     |      _   ||    _____              |      |   |       |               __   __    \
     |     |_|  ||   |__|__|    _______  |      |   |       |    ____      |  | |  |    |
     |  __      ||   |__|__|   |   |   | |      |   |       |   |    |     |  | |  |    |
     | | .|     ||             |  .|.  | |      |   |       |   |   .|     |__| |__|    |
     |_|__|_____||_____________|___|___|_|      |   |       |___|____|__________________|
-}


-- Gamestate, to keep track of what is happening in the game
data GameState = GameState {
                   charPressed  :: CharPressed
                 , elapsedTime :: Float
                 , pacman :: Player
                 , lives :: Int
                 , board :: Board
                 , ghosts :: [Ghost]
                 , score :: Int
                 , paused :: Bool
                 , highScore :: Int
                 , fruitGenerated :: Bool
                 , pelletsLeft :: Int
                 , scareTimer :: Float
                 }

-- Predefined gamestate, to start the game
initialState :: Int -> GameState
initialState highscore = GameState None {- <- no key being pressed-}
                         0 {- <- reset time counter-}
                         initialPlayer {- <- base case player/pacman-}
                         3 {- <- player lives-}
                         initialBoard {- <- base case board-}
                         [blinky,pinky,inky,clyde] {- <- list of ghosts-}
                         0 {- <- reset score-}
                         False {- <- game is not paused-}
                         highscore {- <- gets passed by IO-}
                         False {- <- whether a fruit has been generated-}
                         244 {- <- pellets left in the grid-}
                         0 {- <- scare timer-}

-- Character currently being pressed
data CharPressed = None
                | Character   Char

-- Corners of the grid
data Corner = TopLeft | TopRight | BottomLeft | BottomRight

-- Corners of the grid, in grid coordinates (ghost corners)
cornerToGrid :: Corner -> GridLocation
cornerToGrid c = case c of
                TopLeft -> (3,-4)
                TopRight -> (27,-4)
                BottomLeft -> (0,30)
                BottomRight -> (27,28)

-- States the (G)hosts and (P)acman can be in
data GHuntState = Hunting | Running | Scatter
   deriving (Eq)
data PHuntState = Eating | NotEating
   deriving (Eq)

-- Direction the entity is looking in
data SpriteDir = Up | Down | Left | Right 
   deriving (Eq, Show)

-- The player/pacman, with its attributes
data Player = Player { playerLocation :: Location
                     , pacManHuntState :: PHuntState -- whether pacman is Eating the ghosts or NotEating
                     , pacmanDirection :: SpriteDir -- direction pacman is currently looking in
                     , pacmanTargetDirection :: SpriteDir -- direction pacman wants to look in (but can't because of walls)
                     , nextTilePellet :: Bool -- whether the tile in front of pacman is a pellet
                     }

-- calculates center of the player/pacman
playerCenter :: Player -> Location
playerCenter (Player {playerLocation = loc@(x,y)}) = (x + (0.5 * entitySize) - 4, y + (0.5 * entitySize) - 4)

-- Initial values of the player/pacman
initialPlayer :: Player
initialPlayer = Player (- (tileSize / 2),- (8 * tileSize)) {- <- startlocation -} 
                       NotEating
                       Model.Left {- <- start direction-} 
                       Model.Left 
                       False

-- The enemies with all their attributes
data Ghost = Ghost { ghostLocation :: Location
                   , ghostHuntState :: GHuntState -- either Hunting, Running or Scatter
                   , identification :: GhostID -- Blinky, Inky, Pinky or Clyde
                   , scatterCorner :: Corner -- corner to go to when in scatter mode, also used to calculate Clyde's target
                   , ghostDirection :: SpriteDir -- direction the ghost is looking in
                   , released :: Bool -- if the ghost has exited the cage
                   , dead :: Bool
                   }

data GhostID = Blinky | Pinky | Inky | Clyde
  deriving (Eq)

-- calculates center of a specified ghost
ghostCenter :: Ghost -> Location
ghostCenter (Ghost {ghostLocation = loc@(x,y)}) = (x + (0.5 * entitySize) - 4 , y + (0.5 * entitySize) - 4 )

-- Amount of points a ghost gives when killed
ghostScore :: Int 
ghostScore = 200

-- Defining the start-attributes of the ghosts
blinky :: Ghost
blinky = Ghost (- (2 * tileSize),tileSize) Hunting Blinky TopRight Up False False

pinky :: Ghost
pinky = Ghost (-tileSize,tileSize) Hunting Pinky TopLeft Up False False

inky :: Ghost
inky = Ghost (0,tileSize) Hunting Inky BottomRight Up False False

clyde :: Ghost
clyde = Ghost (tileSize,tileSize) Hunting Clyde BottomLeft Up False False

-- width and height of a tile
tileSize :: Float
tileSize = 24

-- radius of entities, in terms of tileSize, to scale when changing the board size
entitySize :: Float
entitySize = tileSize/3

-- Board representation
data Tile = Wall WallTypes 
          | Tile OpenTileTypes 
          | Void
   deriving (Eq)

-- Tiles that can be walked on
data OpenTileTypes = Empty | Pellet | Power | Fruit
   deriving (Eq)

-- Tiles that cannot be walked on
data WallTypes = Solid | GhostDoor
   deriving (Eq)

-- Type Row and Board using Data.Vector
type Row  = Data.Vector.Vector Tile 
type Board  = Data.Vector.Vector Row

-- Initial board
initialBoard :: Board
initialBoard = Data.Vector.fromList (map Data.Vector.fromList [rowType0, rowType1, rowType2 normal, rowType2 super, rowType2 normal, rowType3, rowType4, rowType4, rowType5, rowType6, rowType6, rowType7, rowType8, rowType9, rowType10, rowType9, rowType11, rowType7, rowType11, rowType11, rowType1, rowType2 normal, rowType2 normal, rowType12 , rowType13, rowType13, rowType5, rowType14, rowType14, rowType3, rowType0])
  where
    normal = Tile Pellet
    super = Tile Power
    rowType0 = replicate 28 (Wall Solid)
    rowType1 =Wall Solid: replicate 12 normal ++ ([Wall Solid, Wall Solid] ++ replicate 12 normal ++ [Wall Solid])
    rowType2 pellet = [Wall Solid, pellet] ++ replicate 4 (Wall Solid)++ [normal] ++ replicate 5 (Wall Solid)++ [normal, Wall Solid, Wall Solid, normal] ++ replicate 5 (Wall Solid)++ [normal] ++ replicate 4 (Wall Solid)++ [pellet, Wall Solid]
    rowType3 =(Wall Solid): replicate 26 normal ++ [Wall Solid]
    rowType4 = [Wall Solid, normal] ++ replicate 4 (Wall Solid)++ [normal, Wall Solid, Wall Solid, normal] ++ replicate 8 (Wall Solid)++ [normal, Wall Solid, Wall Solid, normal] ++ replicate 4 (Wall Solid)++ [normal, Wall Solid]
    rowType5 =Wall Solid: replicate 6 normal ++ [Wall Solid,Wall Solid] ++ replicate 4 normal ++ [Wall Solid,Wall Solid] ++ replicate 4 normal ++ [Wall Solid,Wall Solid] ++ replicate 6 normal ++ [Wall Solid]
    rowType6 = replicate 6 (Wall Solid)++ [normal] ++ replicate 5 (Wall Solid)++ [Tile Empty, Wall Solid, Wall Solid, Tile Empty] ++ replicate 5 (Wall Solid)++ [normal] ++ replicate 6 (Wall Solid)
    rowType7 = replicate 6 (Wall Solid)++ [normal, Wall Solid, Wall Solid] ++ replicate 10 (Tile Empty) ++ [Wall Solid, Wall Solid, normal] ++ replicate 6 (Wall Solid)
    rowType8 = replicate 6 (Wall Solid)++ [normal, Wall Solid, Wall Solid, Tile Empty, Wall Solid, Wall Solid, Wall Solid, {-ghost door -> -}Wall GhostDoor, Wall GhostDoor{- <- ghost door-}, Wall Solid, Wall Solid, Wall Solid, Tile Empty, Wall Solid, Wall Solid, normal] ++ replicate 6 (Wall Solid)
    rowType9 = replicate 6 (Wall Solid)++ [normal, Wall Solid, Wall Solid, Tile Empty, Wall Solid] ++ replicate 6 Void ++ [Wall Solid, Tile Empty, Wall Solid, Wall Solid, normal] ++ replicate 6 (Wall Solid)
    rowType10 = replicate 6 (Tile Empty) ++ [normal] ++ replicate 3 (Tile Empty) ++ [Wall Solid] ++ replicate 6 Void ++ [Wall Solid] ++ replicate 3 (Tile Empty) ++ [normal] ++ replicate 6 (Tile Empty)
    rowType11 = replicate 6 (Wall Solid)++ [normal, Wall Solid, Wall Solid, Tile Empty] ++ replicate 8 (Wall Solid)++ [Tile Empty, Wall Solid, Wall Solid, normal] ++ replicate 6 (Wall Solid)
    rowType12 = [Wall Solid, super, normal, normal, Wall Solid, Wall Solid] ++ replicate 7 normal ++ [Tile Empty, Tile Empty] ++ replicate 7 normal ++ [Wall Solid, Wall Solid, normal, normal, super, Wall Solid]
    rowType13 = [Wall Solid, Wall Solid, Wall Solid, normal, Wall Solid, Wall Solid, normal, Wall Solid, Wall Solid, normal] ++ replicate 8 (Wall Solid)++ [normal, Wall Solid, Wall Solid, normal, Wall Solid, Wall Solid, normal, Wall Solid, Wall Solid, Wall Solid]
    rowType14 = [Wall Solid, normal] ++ replicate 10 (Wall Solid)++ [normal, Wall Solid, Wall Solid, normal] ++ replicate 10 (Wall Solid)++ [normal, Wall Solid]

type Location = (Float,Float)
type GridLocation = (Int,Int)

-- Calculate location on the screen, from a specified cell in the grid
gridCoordinateToScreenCoordinate :: GridLocation -> Location
gridCoordinateToScreenCoordinate (locx,locy) = (-348 + (0.5 * tileSize) + (tileSize * int2Float locx ), 348 + (0.5 * tileSize) - (tileSize * int2Float locy ) )

-- Inverse of the previous; calculate cell in the grid, from a location on the screen
screenCoordinateToGridCoordinate:: Location -> GridLocation
screenCoordinateToGridCoordinate (locx, locy)= (abs ((float2Int locx + 348) `div` float2Int tileSize) , abs ((float2Int locy - 348)`div` float2Int tileSize))

highScoreFilePath :: String
highScoreFilePath = "highscore.txt"

writeHighScore :: Int -> IO ()
writeHighScore highscore = writeFile highScoreFilePath (show highscore)

readHighScore :: IO Int
readHighScore = do
  contents <- readFile highScoreFilePath
  return (read contents)