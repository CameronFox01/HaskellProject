module Main where
import Planets
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace (trace)

-- Initial planets with some velocity
myPlanets :: [Planet]
myPlanets = [
    Planet 20 red 0 60 10 5,
    Planet 50 green 0 0 6 6,
    Planet 30 yellow 0 (-70) (-6) 5]

-- Main function to run the program
main :: IO ()
main = play
    (InWindow "Planets" (1000, 1000) (100, 100)) -- Window size and position
    black                                        -- Background color
    60 -- Frame rate
    myPlanets -- Initial state
    renderFrame -- Renders each frame
    handleEvent -- Event handling, no change to the state in this case
    updateSimState -- Updates each frame

-- Function to render the planets
renderFrame :: [Planet] -> Picture
renderFrame = planetsToPicture


-- Click event handler to spawn a new planet, Will start with 0 velocity
handleEvent :: Event -> [Planet] -> [Planet]
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) ps =
    (Planet 20 (dark red) x y 0 0) : ps

handleEvent _ ps = ps -- Otherwise, don't change state


-- Function to update the planet positions over time
updateSimState :: Float -> [Planet] -> [Planet]
updateSimState time ps = map go ps
  where
    -- Should be < 1.0, Higher value yield faster velocities
    velocityDampeningFactor = 0.07

    go planet = Planet
       (radius planet)
       (pColor planet)
       (xCoor planet + velocityX planet * velocityDampeningFactor)  -- Update x-coordinate based on velocity
       (yCoor planet + velocityY planet * velocityDampeningFactor)  -- Update y-coordinate based on velocity
       (velocityX planet) -- Constant velocity
       (velocityY planet) -- Constant velocity        