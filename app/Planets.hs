module Planets where
import Graphics.Gloss

data Planet = Planet {
    radius :: Float,    -- Radius of the planet
    pColor :: Color,    -- Color of the planet
    xCoor :: Float,     -- x-coordinate of the planet
    yCoor :: Float,     -- y-coordinate of the planet
    velocityX :: Float, -- Horizontal velocity
    velocityY :: Float  -- Vertical velocity
}

-- Converts a planet to a picture
planetToPicture :: Planet -> Picture
planetToPicture p = 
  Translate (xCoor p) (yCoor p) $ 
  Color (pColor p) $ 
  ThickCircle 0 (radius p)

-- Converts all planets to pictures
planetsToPicture :: [Planet] -> Picture
planetsToPicture ps = Pictures [ planetToPicture p | p <- ps ]