-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import GHC.Float (float2Int, int2Float, powerDouble)
import Data.Vector
import Control.Exception (getMaskingState)
import Model
import Data.Vector.Mutable as MV
import GHC.ST (ST(ST), runST)
import Text.Printf (errorBadArgument)
import GHC.Real (infinity)
import Data.Maybe (isJust, fromJust)
import Data.Ord (comparing)
import Data.List (minimumBy)
import qualified GHC.Read as Data
import Debug.Trace (trace,traceShow)
import Model (GameState)

-- Handle one iteration of the game, what has to be changed to the gamestate each tick
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState {paused = psd, lives = lvs, score = sc, highScore = hs, fruitGenerated = fruitgenerated, pelletsLeft = pelletsleft, elapsedTime = time})
  | lvs == 0 = do
    condition <- return (sc > hs)
    if condition
      then
        writeHighScore sc
      else
        return ()
    nhs <- readHighScore
    return (initialState nhs) --after losing all lives, handle the highscore possibly updating, and reset the game
  | not psd && (pelletsleft < 100) && (fruitgenerated == False) = (generateBonusFruit (purestep gstate)) -- fruit gets generated
  | pelletsleft == 0 = return gstate {lives = 0} -- end game when player collects all pellets
  | not psd = return $ purestep gstate -- if the game is not paused, update everything
  | otherwise = return $ handleInput gstate -- What to do when paused
  where 
    purestep = (chase . checkPellets . checkCollision . tickPlayerMovement . handleInput . checkScareTimer . (addTime secs))

-- Check if pacman collides with a ghost, and end the game if so
checkCollision :: GameState -> GameState
checkCollision gstate@(GameState {pacman = p@(Player {playerLocation = ploc, pacManHuntState = state}), lives = l, ghosts = ghsts, score = scr})
  | state == Eating = gstate {ghosts = Prelude.map (fst) checkedGhostDeaths, score = scr + addedScore} -- needs to be done
  | otherwise = if Prelude.any (collision ploc) ghsts then gstate {lives = l - 1} else gstate
  where
    collision :: Location -> Ghost -> Bool --returns true if a ghost collides with a certain tile
    collision _ (Ghost {dead = True}) = False
    collision (x,y) (Ghost {ghostLocation = (gx,gy)}) = abs (x - gx) < (tileSize * 0.5) && abs (y - gy) < (tileSize * 0.5) --should return false if the ghost is dead
    checkGhostDeath :: Ghost -> (Ghost, Int) --check if a ghost dies due to contact with pacman 
    checkGhostDeath ghst = if collision ploc ghst && not (dead ghst) then (killGhost ghst, ghostScore) else (ghst, 0)
    checkedGhostDeaths = Prelude.map checkGhostDeath ghsts
    addedScore = Prelude.sum (Prelude.map snd checkedGhostDeaths)
    killGhost ghst = ghst {dead = True}

--Keep tabs on the hunting timer and change pacman's state appropriately
checkScareTimer :: GameState -> GameState
checkScareTimer gstate@(GameState {elapsedTime = currentTime , scareTimer = scareTime, pacman = player, ghosts = ghsts}) 
  | currentTime >= scareTime = gstate{pacman = depowerPacMan player, ghosts = Prelude.map unScareGhost ghsts} -- end pacman's hunting state
  | otherwise = gstate -- keep hunting

-- Revert the ghosts to their normal state
unScareGhost :: Ghost -> Ghost
unScareGhost ghst = ghst{ghostHuntState = Hunting}

-- Revert pacman to normal state
depowerPacMan :: Player -> Player
depowerPacMan pacman = pacman{pacManHuntState = NotEating}

-- Mess with the space-time continuum
addTime :: Float -> GameState -> GameState
addTime timeToAdd gstate@(GameState{elapsedTime = currentTime}) = gstate{elapsedTime = currentTime + timeToAdd}

-- Check if pacman is on a tile with a pellet, remove the pellet and add the score
checkPellets :: GameState -> GameState
checkPellets gstate@(GameState {pacman = p@(Player {playerLocation = ploc,pacmanDirection = dir}), board = brd, score = scor, pelletsLeft = pelletcount, ghosts = ghsts})
    | pacmanTileType == Tile Pellet = gstate {board = replaceTile brd (x, y) (Tile Empty), score = scor + 10, pelletsLeft = pelletcount - 1} --eating a normal pellet
    | pacmanTileType == Tile Power = gstate {board = replaceTile brd (x, y) (Tile Empty), score = scor + 50, pelletsLeft = pelletcount - 1, ghosts = scaredGhosts, scareTimer = elapsedTime gstate + 6 , pacman = playerHunting} --eating a power pellets
    | pacmanTileType == Tile Fruit = gstate {board = replaceTile brd (x, y) (Tile Empty), score = scor + 200} -- eating a bonus fruit
    | eatable (brd ! ny ! nx) = gstate {pacman = p {nextTilePellet = True}} --when on an empty tile, look ahead one tile to see if we need to play the eating animation
    | otherwise = gstate {pacman = p {nextTilePellet = False}} -- Not a pellet; continue
  where
    eatable a = (a == Tile Pellet) || (a == Tile Power) || (a == Tile Fruit)
    pacmanTileType = brd ! y ! x --the type of tile pacman is on
    scaredGhosts = Prelude.map (reverseGhostDirection. scareGhost) ghsts -- TODO er wordt wel niet gechecked of ze al scared zijn, misschien later nog iets voor toevoegen
    playerHunting = p{pacManHuntState = Eating}
    (x,y) = screenCoordinateToGridCoordinate ploc
    (nx, ny) = nextGridCoord (x,y) dir

reverseGhostDirection :: Ghost -> Ghost
reverseGhostDirection gst@(Ghost{dead = True}) = gst
reverseGhostDirection gst@(Ghost{ghostDirection = dir}) = gst {ghostDirection = oppositeDirection dir}


-- Boo
scareGhost :: Ghost -> Ghost
scareGhost gst = gst{ghostHuntState = Running}

-- Handle user input
input :: Event -> GameState -> IO GameState
input e@(EventKey {}) gstate  = return (inputKey e gstate)
input _ gstate = return gstate -- Does nothing, prevents non-exhaustive pattern

-- Handle key input
inputKey :: Event -> GameState -> GameState
inputKey (EventKey key@(Char c) Graphics.Gloss.Interface.IO.Game.Down _ _) gstate = gstate { charPressed = Character c } -- If the user presses a key, pass it to the gamestate to be handled
inputKey (EventKey (SpecialKey KeyEsc) Graphics.Gloss.Interface.IO.Game.Down _ _) gstate = error "The only pain I want in my life is pain au chocolat." -- close the game on Esc-press
inputKey (EventKey key@(Char 'p') Graphics.Gloss.Interface.IO.Game.Up _ _) gstate@(GameState {paused = psd}) = gstate {paused = not psd} -- pause/unpause the game when 'p' is pressed
inputKey _ gstate = gstate -- otherwise, do nothing

-- Do something with the pressed key
handleInput :: GameState -> GameState
-- Player is trying to change their direction
handleInput  gstate@(GameState {charPressed = (Character 'w'), pacman = p}) = gstate {pacman = p {pacmanTargetDirection = Model.Up}, charPressed = None}
handleInput  gstate@(GameState {charPressed = (Character 'a'), pacman = p}) = gstate {pacman = p {pacmanTargetDirection = Model.Left}, charPressed = None}
handleInput  gstate@(GameState {charPressed = (Character 's'), pacman = p}) = gstate {pacman = p {pacmanTargetDirection = Model.Down}, charPressed = None}
handleInput  gstate@(GameState {charPressed = (Character 'd'), pacman = p}) = gstate {pacman = p {pacmanTargetDirection = Model.Right}, charPressed = None}

handleInput  gstate@(GameState {charPressed = None}) = gstate --no input was registered, nothing should be changed
handleInput  gstate = gstate

-- Tick the player movement
tickPlayerMovement :: GameState -> GameState
tickPlayerMovement gstate@(GameState {pacman = p@(Player screencoord@(x,y) _ dir tdir _), board = brd})
  | (dir /= tdir) && canTurnHere && playerCentered = movePlayer (gstate {pacman = p {pacmanDirection = tdir}}) -- Trying to turn, and can turn, thus turn
  | isOppositeTurn = tickPlayerMovement (gstate {pacman = p {pacmanDirection = tdir}}) -- Trying to make a 180, recursively check if the turn is possible
  | nextTileIsWall && playerCentered = gstate -- Trying to go to a wall, thus stand still
  | otherwise = movePlayer gstate -- Not turning or running into a wall, simply walk
  where
    playerGridCoords = screenCoordinateToGridCoordinate screencoord -- Cell the player is on
    nextTile = tileDirection brd playerGridCoords dir
    nextTileIsWall = (nextTile == Wall Solid) || (nextTile == Wall GhostDoor)
    canTurnHere = (tileDirection brd playerGridCoords tdir /= Wall Solid) && (tileDirection brd playerGridCoords tdir /= Wall GhostDoor) -- Whether the tile where the player is trying to go is a wall or not
    playerCentered = isPlayerCentered p playerGridCoords -- If the player is in the center of the tile
    isOppositeTurn = oppositeDirection dir == tdir -- If the player is making a 180

-- Actually move the sprite of the player
movePlayer :: GameState -> GameState
movePlayer gstate@(GameState {pacman = p@(Player (x,y) _ dir _ _)}) =
  case dir of
  Model.Up -> gstate {pacman = p {playerLocation = (x,y + 1)}} -- Going up, change y-coord
  Model.Left -> if x <= - (tileSize * 14)  -- check if the player is going through the teleport-tunnel
               then gstate {pacman = p {playerLocation = (tileSize * 13, y)}} -- teleport to the other side
               else gstate {pacman = p {playerLocation = (x - 1,y)}} -- simply move left
  Model.Right -> if x >= tileSize * 13  -- check if the player is going through the teleport-tunnel
               then gstate {pacman = p {playerLocation = (-(tileSize * 14), y)}} -- teleport to the other side
               else gstate {pacman = p {playerLocation = (x + 1,y)}} -- simply move right
  Model.Down -> gstate {pacman = p {playerLocation = (x,y - 1)}} -- Going down, change y-coord

-- Returns the type of next tile in the direction from the given coordinate
tileDirection ::  Board -> (Int,Int) -> SpriteDir -> Tile
tileDirection board (x,y) Model.Up = (board ! (y - 1)) ! x
tileDirection board (x,y) Model.Left | x > 0 = (board ! y) ! (x - 1) -- If the location is at the end of the left tunnel, check the end of the right tunnel
                                    | otherwise = (board ! y) ! 27
tileDirection board (x,y) Model.Right | x < 27 = (board ! y) ! (x + 1)  -- If the location is at the end of the right tunnel, check the end of the left tunnel
                                     | otherwise = (board ! y) ! 0
tileDirection board (x,y) Model.Down = (board ! (y + 1)) ! x

-- Gets the next grid-coordinate in a certain direction
nextGridCoord :: GridLocation -> SpriteDir -> (Int,Int)
nextGridCoord (x,y) dir =
  case dir of
    Model.Up -> (x ,y - 1)
    Model.Left -> if x <= 0 then (27,y) else (x -1 ,y)
    Model.Right -> if x >= 27 then (0,y) else (x + 1, y)
    Model.Down ->  (x, y + 1)

-- Replaces the tile on the given coordinate with the specified tile 
replaceTile :: Board -> GridLocation -> Tile -> Board
replaceTile brd (x,y) tile = runST $ do
  bd <- thaw brd -- "Open" the board
  row <- thaw (brd ! y) -- "Open" the row
  write row x (tile) -- Change the specified cell to an empty tile
  nrow <- freeze row -- "Close" the row
  write bd y nrow -- Change the row in the board
  freeze bd -- "Close" the board

-- Determines if pacman is centered
isPlayerCentered :: Player -> GridLocation -> Bool
isPlayerCentered p@(Player {playerLocation = ploc@(x,y)}) gloc = playerCenter p == gridCoordinateToScreenCoordinate gloc

-- Invert direction
oppositeDirection :: SpriteDir -> SpriteDir
oppositeDirection dir =
    case dir of
    Model.Up -> Model.Down
    Model.Left -> Model.Right
    Model.Right -> Model.Left
    Model.Down -> Model.Up

-- Move the ghosts towards their target
chase :: GameState -> GameState
chase gstate@(GameState {ghosts = ghosts}) = gstate {ghosts = newghosts}
  where
    newghosts =  Prelude.map (tickGhostMovement gstate) ghosts

-- Update the ghosts behaviour each tick
tickGhostMovement :: GameState -> Ghost -> Ghost
tickGhostMovement gstate@(GameState {board = brd, pacman = player, score = sc} ) ghost@(Ghost {ghostLocation = loc, ghostDirection = dir, released = rld})
  | dead ghost && isGhostCentered_ && gridlocation == doorLoc && rld = moveGhost (ghost {ghostDirection = Model.Down, released = False}) --move the ghost down into the ghost house, if they are dead and on the doortile
  | not rld && gridlocation == doorLoc && isGhostCentered_ = tickGhostMovement gstate ghost {released = True, dead = False} -- ghost has exited the cage, can now start chasing pacman
  | isGhostCentered_ && isIntersection_ = moveGhost (ghost {ghostDirection = findTurnDirection adjacentCoords adjacent ghostTarget dir} ) -- Calculate where to go at the intersection, and move in that direction, will not do 180s
  | nextTileIsWall && isGhostCentered_ = moveGhost (ghost {ghostDirection = findTurnDirection adjacentCoords adjacent ghostTarget dir}) -- If there is a wall, look which direction to go
  | not rld = case identification ghost of -- If not yet out of the box, target that first
                    Blinky -> moveGhost ghost
                    Pinky -> if sc > 150 then moveGhost ghost else ghost
                    Inky -> if sc > 300 then moveGhost ghost else ghost
                    Clyde -> if sc > 450 then moveGhost ghost else ghost
  | otherwise = moveGhost ghost -- Nothing? Just go forward
  where
    adjacent = adjacentTiles brd gridlocation -- All adjacent tiles
    adjacentCoords = adjacentCoordinates brd gridlocation
    isGhostCentered_ = isGhostCentered ghost gridlocation -- Check if the ghost is centered
    gridlocation = screenCoordinateToGridCoordinate loc
    isIntersection_ = isIntersection adjacent
    nextTile = tileDirection brd gridlocation dir -- The next tile if the ghost keeps moving towards the direction they are currently facing 
    nextTileIsWall = (nextTile == Wall Solid) || (nextTile == Wall GhostDoor) -- Check if there is a wall
    ghostTarget | not rld = doorLoc -- not released, target to the cage exit first
                | dead ghost = doorLoc --where to go if dead
                | otherwise = targetLoc (identification ghost) gstate -- Determine target location
    doorLoc = (13,11)

-- Determine which way to turn to get closer to pacman
findTurnDirection :: (GridLocation,GridLocation,GridLocation,GridLocation) -> (Tile,Tile,Tile,Tile) -> GridLocation  -> SpriteDir -> SpriteDir
findTurnDirection (upCoord,rightCoord,downCoord,leftCoord)  (upTile, rightTile, downTile, leftTile) target currentDirection = getLowestDistance collection2
  where
    upLength = (getTilePlayerLength upCoord upTile target, Model.Up)
    rightLength = (getTilePlayerLength rightCoord rightTile target, Model.Right)
    downLength = (getTilePlayerLength downCoord downTile target, Model.Down)
    leftLength = (getTilePlayerLength leftCoord leftTile target, Model.Left)
    lst = [upLength,rightLength,downLength,leftLength]
    collection = Prelude.filter (isJust . fst) lst --a collection of all the valid tiles and their distance (solid tiles are discarded) 
    collection2 = Prelude.filter ((/=) (oppositeDirection currentDirection) . snd) collection -- discard the 180 tile
    correctDirection = getLowestDistance collection2

-- Helper function for findTurnDirection
getLowestDistance :: [(Maybe Float, SpriteDir)] -> SpriteDir
getLowestDistance lst = snd (Data.List.minimumBy (comparing (fromJust . fst)) lst)

-- Calculates the direction between a tile and a location, returns nothing if the tile is solid
getTilePlayerLength :: GridLocation -> Tile -> GridLocation -> Maybe Float
getTilePlayerLength _ (Wall Solid) _ = Nothing
getTilePlayerLength (gx,gy) _ (tx,ty) = Just (sqrt (((fromIntegral gx - fromIntegral tx)^2) + ((fromIntegral gy - fromIntegral ty)^2)))

-- Returns whether the ghost is centered
isGhostCentered :: Ghost -> GridLocation -> Bool
isGhostCentered ghst@(Ghost {ghostLocation = loc@(x,y)}) gloc = ghostCenter ghst == gridCoordinateToScreenCoordinate gloc

-- Move a ghost based on their current direction
moveGhost :: Ghost -> Ghost
moveGhost ghost@(Ghost {ghostLocation = (x,y), ghostDirection = dir}) =
  case dir of
  Model.Up -> ghost {ghostLocation = (x,y + 1)} -- Going up, change y-coord
  Model.Left -> if x <= - (tileSize * 14)  -- check if the ghost is going through the teleport-tunnel
               then ghost {ghostLocation = (tileSize * 13, y)} -- teleport to the other side
               else ghost {ghostLocation = (x - 1,y)} -- simply move left
  Model.Right -> if x >= tileSize * 13   -- check if the ghost is going through the teleport-tunnel
               then ghost {ghostLocation = (-(tileSize * 14), y)} -- teleport to the other side
               else ghost {ghostLocation = (x + 1,y)} -- simply move right
  Model.Down -> ghost {ghostLocation = (x,y - 1)} -- Going down, change y-coord

-- Target location of each ghost
targetLoc :: GhostID -> GameState -> GridLocation
-- Blinky simply targets pacman
targetLoc Blinky (GameState {pacman = (Player {playerLocation = (x,y)})}) = newGridLoc
  where
    newGridLoc@(nx,ny) = screenCoordinateToGridCoordinate (x,y)
-- Pinky targets 4 tiles ahead of pacman (the bug in the original Pac-Man has been fixed here)
targetLoc Pinky (GameState {pacman = (Player {playerLocation = (x,y), pacmanDirection = dir})}) | dir == Model.Up = (fst loc, snd loc - 4)
                                                                                                | dir == Model.Down = (fst loc, snd loc + 4)
                                                                                                | dir == Model.Left = (fst loc - 4, snd loc)
                                                                                                | dir == Model.Right = (fst loc + 4, snd loc)
                                                                                                where loc = screenCoordinateToGridCoordinate (x,y)
-- Inky targets the cell which is twice the vector between Blinky and two tiles in front of pacman
targetLoc Inky (GameState {pacman = (Player {playerLocation = (x,y), pacmanDirection = dir}), ghosts = gs}) = newGridLoc
                                                                                              where
                                                                                                newGridLoc@(nx,ny) = screenCoordinateToGridCoordinate newLoc
                                                                                                -- Calculated cell
                                                                                                newLoc = (bx + 2*vx, by + 2*vy)
                                                                                                -- Vector between Blinky and twoInFrontOf
                                                                                                (vx,vy) = (fst twoInFrontOf - bx,snd twoInFrontOf - by)
                                                                                                -- Get Blinky's location
                                                                                                (Ghost (bx,by) _ _ _ _ _ _) = Prelude.head (Prelude.filter (\(Ghost {identification = id}) -> id == Blinky) gs)
                                                                                                -- Two tiles in front of pacman
                                                                                                twoInFrontOf | dir == Model.Up = (fst loc, snd loc - 2)
                                                                                                             | dir == Model.Down = (fst loc, snd loc + 2)
                                                                                                             | dir == Model.Left = (fst loc - 2, snd loc)
                                                                                                             | dir == Model.Right = (fst loc + 2, snd loc)
                                                                                                             where loc = (x,y)

-- Clyde targets pacman, but if he's within 8 tiles of pacman, he runs to his corner
targetLoc Clyde (GameState {pacman = (Player {playerLocation = (px,py)}), ghosts = gs}) | distance < (8 * tileSize) = cornerLoc
                                                                                        | otherwise = newGridLoc
                                                                                        where
                                                                                          -- Simple math to determine the distance
                                                                                          newGridLoc@(nx,ny) = screenCoordinateToGridCoordinate (px,py)
                                                                                          distance = sqrt ((dx * dx) + (dy * dy))
                                                                                          dx = px - cx
                                                                                          dy = py - cy
                                                                                          (Ghost (cx,cy) _ _ corner _ _ _) = Prelude.head (Prelude.filter (\(Ghost {identification = id}) -> id == Clyde) gs)
                                                                                          cornerLoc@(kx,ky) = cornerToGrid corner


adjacentTiles :: Board -> GridLocation -> (Tile,Tile,Tile,Tile)
adjacentTiles brd loc = (tileDirection brd loc Model.Up, tileDirection brd loc Model.Right, tileDirection brd loc Model.Down, tileDirection brd loc Model.Left)

adjacentCoordinates :: Board -> GridLocation ->(GridLocation,GridLocation,GridLocation,GridLocation)
adjacentCoordinates brd loc = (nextGridCoord loc Model.Up, nextGridCoord loc Model.Right,nextGridCoord loc Model.Down,nextGridCoord loc Model.Left)

-- Returns whether this tile is an intersection (more than 2 ways to go)
isIntersection :: (Tile,Tile,Tile,Tile) -> Bool
isIntersection (up,right,down,left) = Prelude.length (Prelude.filter isOpenTile [up,right,down,left]) > 2

isOpenTile :: Tile -> Bool
isOpenTile (Tile _) = True
isOpenTile Void = True
isOpenTile _        = False

-- Will place a bonus fruit on a random empty tile
generateBonusFruit :: GameState -> IO GameState
generateBonusFruit gstate@(GameState{board = brd}) = do
  (tile, coordinate) <- getRandomTile gstate
  if tile == Tile Empty
    then
      return (gstate {board = replaceTile brd coordinate (Tile Fruit), fruitGenerated = True})
    else
      generateBonusFruit gstate

-- Gives a random coordinate along with the type of tile present there
getRandomTile :: GameState -> IO (Tile, GridLocation)
getRandomTile GameState{board = brd} = do
  generator <- newStdGen
  let (randomX, _) =  randomR (1 :: Int, 26) generator -- Indexes for the vector list of the board, 1 - 26 excludes the outermost coordinates
  let (randomY, _) = randomR (1 :: Int, 26) generator
  let tiletype = brd ! randomY ! randomX
  return (tiletype, (randomX, randomY))