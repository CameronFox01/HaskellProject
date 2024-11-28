module Main where
import Planets
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace (trace)

-- Initial planets with some velocity
myPlanets :: [Planet]
myPlanets = [
    Planet 20 red 0 60 0 1 500,
    Planet 50 yellow 0 0 0 0 10000,
    Planet 30 green 0 (-70) (-1) 0 800]

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
    (Planet 20 (dark red) x y 0 0 100) : ps

handleEvent _ ps = ps -- Otherwise, don't change state


-- Function to update the planet positions over time
updateSimState :: Float -> [Planet] -> [Planet]
updateSimState time ps = trace (show forces) $ map updatePlanet ps
  where
    velocityDampeningFactor = 1.07 -- 0.07
    forces = [netForce p ps | p <- ps] -- Debugging forces

    updatePlanet planet = 
      let (fx, fy) = netForce planet ps
          ax = fx / gravity planet -- Acceleration in x
          ay = fy / gravity planet -- Acceleration in y
          vx' = velocityX planet + ax * time
          vy' = velocityY planet + ay * time
          x' = xCoor planet + vx' * velocityDampeningFactor
          y' = yCoor planet + vy' * velocityDampeningFactor
      in planet { xCoor = x', yCoor = y', velocityX = vx', velocityY = vy' }
{-
    go planet = Planet
       (radius planet)
       (pColor planet)
       (xCoor planet + velocityX planet * velocityDampeningFactor)  -- Update x-coordinate based on velocity
       (yCoor planet + velocityY planet * velocityDampeningFactor)  -- Update y-coordinate based on velocity
       (velocityX planet) -- Constant velocity
       (velocityY planet) -- Constant velocity        
       (gravity planet) --constant gravity
-}





