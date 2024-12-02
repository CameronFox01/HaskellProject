module Planets where
import Graphics.Gloss

data Planet = Planet {
    radius :: Float,    -- Radius of the planet
    pColor :: Color,    -- Color of the planet
    xCoor :: Float,     -- x-coordinate of the planet
    yCoor :: Float,     -- y-coordinate of the planet
    velocityX :: Float, -- Horizontal velocity
    velocityY :: Float -- Vertical velocity
} deriving (Eq)

gravitationalConstant :: Float
gravitationalConstant = 25 -- Arbitrary small scale for simulation 1e-1

-- Converts a planet to a picture
planetToPicture :: Planet -> Picture
planetToPicture p = 
  Translate (xCoor p) (yCoor p) $ 
  Color (pColor p) $ 
  ThickCircle 0 (radius p)

-- Converts all planets to pictures
planetsToPicture :: [Planet] -> Picture
planetsToPicture ps = Pictures [ planetToPicture p | p <- ps ]

-- Calculate distance between two planets
distance :: Planet -> Planet -> Float
distance p1 p2 = sqrt ((xCoor p1 - xCoor p2)^2 + (yCoor p1 - yCoor p2)^2)

-- Single Planets gravitation force
planetToGravitationalForce :: Planet -> Float
planetToGravitationalForce p = radius p * 25

-- Gravitational force between two planets as a vector (x, y components)
gravitationalForce :: Planet -> Planet -> (Float, Float)
gravitationalForce p1 p2
    | dist == 0 = (0, 0) -- Avoid division by zero
    | otherwise = (force * dx / dist, force * dy / dist) 
  where
    dist = distance p1 p2
    dx = xCoor p2 - xCoor p1
    dy = yCoor p2 - yCoor p1
    force = gravitationalConstant * planetToGravitationalForce p1 * planetToGravitationalForce p2 / dist^2

-- Sum up forces from all planets acting on a single planet
netForce :: Planet -> [Planet] -> (Float, Float)
netForce p ps =  foldl addForces (0, 0) [gravitationalForce p other | other <- ps, other /= p]
  where
    addForces (fx, fy) (fx', fy') = (fx + fx', fy + fy')
