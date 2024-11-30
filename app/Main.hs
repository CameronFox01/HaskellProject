module Main where
import Planets
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace (trace)

data SimState = SimState {
  planets :: [Planet],
  gravityInput :: String,
  gravityValue :: Float
}

-- Initial planets with some velocity
myPlanets :: [Planet]
myPlanets = [
    Planet 20 red 0 260 (-1) (-1) 200,
    Planet 50 blue 0 0 0 0 2500,
    Planet 30 green 0 (-150) (1) 1 400]

initialState :: SimState
initialState = SimState {
  planets = myPlanets,
  gravityInput = "100",
  gravityValue = 100
}

-- Main function to run the program
main :: IO ()
main = play
    (InWindow "Planets" (1000, 1000) (100, 100)) -- Window size and position
    black                                        -- Background color
    60 -- Frame rate
    initialState -- Initial state
    renderFrame -- Renders each frame
    handleEvent -- Event handling, no change to the state in this case
    updateSimState -- Updates each frame

-- Function to render the planets
renderFrame :: SimState -> Picture
renderFrame simState = 
  pictures [planetsToPicture (planets simState), renderTextBox (gravityInput simState)]

-- Render the text box with the current gravity input
renderTextBox :: String -> Picture
renderTextBox input = translate (-400) (-450) $ -- Position at bottom-left corner
    pictures [ color black $ rectangleSolid 200 50           -- Background
             , color white $ translate (-90) (-10) $ scale 0.15 0.15 $ text input -- Text
             ]

-- Click event handler to spawn a new planet, Will start with 0 velocity
handleEvent :: Event -> SimState -> SimState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) simState =
    let newPlanet = Planet 20 (dark red) x y 0 0 (gravityValue simState)
    in simState { planets = newPlanet : planets simState }

-- Handle keyboard input for modifying gravity
handleEvent (EventKey (Char c) Down _ _) simState
  | c `elem` ['0'..'9'] = simState { gravityInput = gravityInput simState ++ [c] } -- Add digit
handleEvent (EventKey (SpecialKey KeyBackspace) Down _ _) simState =
  simState { gravityInput = init (gravityInput simState) } -- Remove last character on Windows (I Think)
handleEvent (EventKey (SpecialKey KeyDelete) Down _ _) simState = 
  simState { gravityInput = init (gravityInput simState)}  -- Remove last character on Mac (For Sure)
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) simState =
  let newGravity = read (gravityInput simState) :: Float
  in simState { gravityValue = newGravity } -- Update gravity value
handleEvent _ simState = simState -- Otherwise, no change


-- Function to update the planet positions over time
updateSimState :: Float -> SimState -> SimState
updateSimState time simState =
  let ps = planets simState
      velocityDampeningFactor = 0.14
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
  in simState { planets = map updatePlanet ps }
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





