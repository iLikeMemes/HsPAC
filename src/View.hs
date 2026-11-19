-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Graphics.Gloss
import GHC.Float
import qualified Data.Vector

view :: GameState -> IO Picture
view = return . viewPure

-- "List" of all things to be drawn
viewPure :: GameState -> Picture
viewPure (GameState _ _ player@(Player loc _ _ _ _) _ board ghosts score paused highScore _ _ _) = Pictures ([drawBoard board, Pictures (map (\g@(Ghost {ghostLocation = l}) -> draw g l) ghosts), draw player loc] {- <- actual game stuff-}
                                                                                          ++ [drawScore score] {- <- draw score-} 
                                                                                          ++ [pauseScreen paused] {- <- pause screen overlay-}
                                                                                          ++ [drawHighScore highScore]) 


-- Pause screen is always drawn, but only shows "Paused" when the game is actually paused
pauseScreen :: Bool -> Picture
pauseScreen True = Color white $ pictures [translate (-200) 0 (Text "Paused")] -- Write "Paused" in the center-ish of the screen
pauseScreen _ = blank -- don't draw anything

-- Every object/entity that can be drawn onto the screen
class Drawable d where
  draw :: d -> Location -> Picture

-- Define drawing the player/pacman
instance Drawable Player where
  draw (Player _ _ dir _ onPellet) (x,y) = pictures [color white (translate x y (thickCircle entitySize entitySize)), color black $ polygon [(x,y),(fstX,fstY),(sndX,sndY)]]
    where
      offset (dx, dy) = (x + dx * entitySize, y + dy * entitySize)
      (fstX, fstY) = case dir of
                       Model.Left  -> if onPellet && even (float2Int (x*0.1)) then offset (-1.8, -1.8) else offset (-1.8,-0.8)
                       Model.Right -> if onPellet && even (float2Int (x*0.1)) then offset (1.8, 1.8) else offset (1.8,0.8)
                       Model.Up    -> if onPellet && even (float2Int (y*0.1)) then offset (-1.8, 1.8) else offset (-0.8,1.8)
                       Model.Down  -> if onPellet && even (float2Int (y*0.1)) then offset (1.8, -1.8) else offset (0.8,-1.8)
      (sndX, sndY) = case dir of
                       Model.Left  -> if onPellet && even (float2Int (x*0.1)) then offset (-1.8, 1.8) else offset (-1.8,0.8)
                       Model.Right -> if onPellet && even (float2Int (x*0.1)) then offset (1.8, -1.8) else offset (1.8,-0.8)
                       Model.Up    -> if onPellet && even (float2Int (y*0.1)) then offset (1.8, 1.8) else offset (0.8,1.8)
                       Model.Down  -> if onPellet && even (float2Int (y*0.1)) then offset (-1.8, -1.8) else offset (-0.8,-1.8)
      (gx,gy) = screenCoordinateToGridCoordinate (x,y)

-- Define drawing the ghosts
instance Drawable Ghost where
  draw (Ghost _ state id _ dir _ dead) (x,y)
    | id == Blinky = if dead then deadColour red else (if state == Running then runningColour else drw red)
    | id == Pinky = if dead then deadColour (makeColor (255/255) (141/255) (161/255) 1) else (if state == Running then runningColour else drw (makeColor (255/255) (141/255) (161/255) 1))
    | id == Inky = if dead then deadColour (makeColor (64/255) (224/255) (208/255) 1) else (if state == Running then runningColour else drw (makeColor (64/255) (224/255) (208/255) 1))
    | id == Clyde = if dead then deadColour orange else (if state == Running then runningColour else drw orange)
    | otherwise = undefined -- will never be used, because all ghosts are defined above
    where 
      runningColour = case dir of -- Animation during Running state
        Model.Left -> if even_ x then drw yellow else drw blue
        Model.Right -> if even_ x then drw yellow else drw blue
        Model.Up -> if even_ y then drw yellow else drw blue
        Model.Down -> if even_ y then drw yellow else drw blue
      deadColour c = color c (translate x y (thickCircle (entitySize*0.5) (entitySize*0.8))) -- Colour of the ghost while moving back to the cage
      drw clr = color clr (translate x y (thickCircle entitySize entitySize)) -- Draw the ghost with a variable colour
      even_ v = even (float2Int (v*0.1)) -- Helper function to determine even or uneven value

-- Define drawing a single tile
instance Drawable Tile where
  draw t (x,y) =
    case t of
      Wall Solid -> color blue (translate xtrans ytrans (rectangleSolid (size*0.8) (size*0.8)))
      Wall GhostDoor -> color (makeColor (150/255) (200/255) (255/255) 1) (translate xtrans ytrans (rectangleSolid (size*0.8) (size*0.8)))
      Tile Empty -> color black (translate xtrans ytrans (rectangleSolid size size))
      Tile Pellet -> color orange (translate xtrans ytrans (circle (size/6)))
      Tile Power -> color white (translate xtrans ytrans (circle (size*0.3)))
      Tile Fruit -> color red (translate xtrans ytrans (circle (size*0.3)))
      Void -> color black (translate xtrans ytrans (rectangleSolid size size)) -- tiles outside the grid
      where
        size = tileSize
        xtrans = (x*size) - xoffset -- translation in the x-direction
        ytrans = -((y*size) - yoffset) -- translation in the y-direction
        xoffset = 14*size -- x-offset of the entire board
        yoffset = 15*size -- y-offset of the entire board


-- Drawing an entire row of tiles
drawRow :: Row -> Float -> Picture
drawRow r y = pictures (Data.Vector.toList (Data.Vector.zipWith (\t x -> draw t (x, y)) r (Data.Vector.fromList [0..])))

-- Drawing the entire board
drawBoard :: Board -> Picture
drawBoard b = pictures (Data.Vector.toList (Data.Vector.zipWith drawRow b (Data.Vector.fromList [0..])))

-- Writes the score
drawScore :: Int -> Picture
drawScore score = scale 0.5 0.5 (Color white (Translate 700 (-400) (Pictures [translate 450 0 (Text (show score)), Text "score:"])))

-- Draws the highscore to the screen
drawHighScore :: Int -> Picture
drawHighScore highScore = scale 0.4 0.4 (Color white (Translate 850 (-700) (Pictures [translate 650 0 (Text (show highScore)), Text "highscore:"])))