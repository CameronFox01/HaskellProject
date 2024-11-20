module Main where
import Graphics.Gloss
import Planets

myPlanets = [(Planet 20 red 0 60), (Planet 50 green 0 0), (Planet 30 yellow 0 (-70))]

-- Main function to run the program
main :: IO ()
main = animate
    (InWindow "Planets" (1000, 1000) (100, 100)) -- Window size and position
    black                                              -- Background color
    (frame myPlanets)                                            -- Frame function

-- Function to generate each frame of the animation
frame :: [Planet] -> Float -> Picture
frame ps time = Pictures [ go p | p <- ps ]
  where
    go planet = Translate x (yCoor planet) $ planetToPicture planet
    x = 100 * sin time -- X-coordinate changes with time
