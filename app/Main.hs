module Main where
import Planets
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace (trace)

data SimState = SimState {
  planets :: [Planet],
  radiusInput :: String,
  xVelocityInput :: String,
  yVelocityInput :: String,
  viewportOffset :: (Float, Float), -- Offset for moving the viewport
  mouseDrag :: Maybe (Float, Float)  -- Mouse drag position, Either Nothing or (x, y)
}

-- Initial planets with some velocity
myPlanets :: [Planet]
myPlanets = [
    --Planet 20 red 0 260 (-1) (-1),
    Planet 150 yellow 0 0 0 0,
    Planet 30 green (-300) (-200) 6 (-5),
    Planet 15 rose (0) (-175) 10 (-10)
    --Planet 40 white 100 (-250) 1 1,
    --Planet 30 rose (-100) 400 (-1) (-1)
    ]

initialState :: SimState
initialState = SimState {
  planets = myPlanets,
  radiusInput = "10",
  xVelocityInput = "0",
  yVelocityInput = "0",
  viewportOffset = (0, 0), -- No offset initially
  mouseDrag = Nothing -- No drag initially
}

-- Main function to run the program
main :: IO ()
main = play
    (InWindow "Planets" (1000, 1000) (100, 100)) -- Window size and position
    black                                        -- Background color
    60 -- Frame rate
    initialState -- Initial state
    renderFrame -- Renders each frame
    handleEvent -- Event handling
    updateSimState -- Updates each frame

-- Function to render the planets
renderFrame :: SimState -> Picture
renderFrame simState = 
  let (vx, vy) = viewportOffset simState
  in pictures [translate vx vy $ planetsToPicture (planets simState), renderTextBox (radiusInput simState) (xVelocityInput simState) (yVelocityInput simState)]

-- Render the text box with the current gravity input
renderTextBox :: String -> String -> String -> Picture
renderTextBox radiusInput vxInput vyInput = translate (-400) (-470) $ -- Position at bottom-left corner
    pictures [ color white $ translate (-90) (-10) $ scale 0.15 0.15 $ 
                                                          text ("New Planet -> Radius: " ++ radiusInput ++ ", X Velocity: " ++ vxInput ++ ", Y Velocity: " ++ vyInput) -- Text
             ]

{-
I dont think the background is necessary since there's so much empty space

    pictures [ color black $ rectangleSolid 250 50           -- Background
             , color white $ translate (-90) (-10) $ scale 0.15 0.15 $ text ("New Planet Radius: " ++ input) -- Text
             ]
-}

viewportDragOffsetAmount :: Float
viewportDragOffsetAmount = 20

-- Handle events
handleEvent :: Event -> SimState -> SimState

-- Right mouse button click to start dragging viewport
handleEvent (EventKey (MouseButton RightButton) Down _ (x, y)) simState =
    simState { mouseDrag = Just (x, y) } -- Start drag position with where mouse is

-- Right mouse button release to stop viewport drag
handleEvent (EventKey (MouseButton RightButton) Up _ _) simState =
    simState { mouseDrag = Nothing } -- Reset drag to nothing

-- Dragging screen with right click
handleEvent (EventMotion (mouseX, mouseY)) simState =
    case mouseDrag simState of
        Just (newMouseX, newMouseY) -> 
            let (viewportX, viewportY) = viewportOffset simState
                viewportdx = mouseX - newMouseX
                viewportdy = mouseY - newMouseY
                
            in simState { viewportOffset = (viewportX + viewportdx, viewportY + viewportdy), mouseDrag = Just (mouseX, mouseY) } -- Move viewport and set mouseDrag location to current location of cursor
        Nothing -> simState

-- Left mouse click to spawn a new planet
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) simState =
    let (vx, vy) = viewportOffset simState
        (adjustedX, adjustedY) = (x - vx, y - vy) -- Adjust for viewport offset
        size = max 10 (read (radiusInput simState)) -- Radius change here
        sizeDebug = trace ("Size of new planet: " ++ show size) size
        newPlanet = Planet sizeDebug (dark red) adjustedX adjustedY (read $ xVelocityInput simState) (read $ yVelocityInput simState)
    in simState { planets = newPlanet : planets simState }

-- Keyboard input for modifying planet size
handleEvent (EventKey (Char c) Down _ _) simState
  | c `elem` ['0'..'9'] = simState { radiusInput = radiusInput simState ++ [c] }

-- Delete key for deleting final character
handleEvent (EventKey (SpecialKey KeyDelete) Down _ _) simState = 
  let currentInput = radiusInput simState
  in if null currentInput
     then simState  -- Don't change state if textfield is empty
     else simState { radiusInput = init currentInput } -- Remove last character

-- Arrow keys for adjusting velocity

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) simState = 
  let currentInput = read (yVelocityInput simState) :: Float
  in simState { yVelocityInput = show (currentInput + 0.5) }

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) simState = 
  let currentInput = read (yVelocityInput simState) :: Float
  in simState { yVelocityInput = show (currentInput - 0.5) }

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) simState = 
  let currentInput = read (xVelocityInput simState) :: Float
  in simState { xVelocityInput = show (currentInput - 0.5) }

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) simState = 
  let currentInput = read (xVelocityInput simState) :: Float
  in simState { xVelocityInput = show (currentInput + 0.5) }


handleEvent _ simState = simState

calculateInertia :: Planet -> Float
calculateInertia p = radius p ** 1.7

-- Function to update the planet positions over time
updateSimState :: Float -> SimState -> SimState
updateSimState time simState =
  let ps = planets simState
      velocityDampeningFactor = 0.15
      forces = [netForce p ps | p <- ps] -- Debugging forces
      updatePlanet planet = 
        let (fx, fy) = netForce planet ps
            ax = fx / calculateInertia planet -- Acceleration in x
            ay = fy / calculateInertia planet -- Acceleration in y
            vx' = velocityX planet + ax * time
            vy' = velocityY planet + ay * time
            x' = xCoor planet + vx' * velocityDampeningFactor
            y' = yCoor planet + vy' * velocityDampeningFactor
        in planet { xCoor = x', yCoor = y', velocityX = vx', velocityY = vy' }
  in simState { planets = map updatePlanet ps }

{-
    go planet = Planet
       (radiusInput planet)
       (pColor planet)
       (xCoor planet + velocityX planet * velocityDampeningFactor)  -- Update x-coordinate based on velocity
       (yCoor planet + velocityY planet * velocityDampeningFactor)  -- Update y-coordinate based on velocity
       (velocityX planet) -- Constant velocity
       (velocityY planet) -- Constant velocity        
       (gravity planet) --constant gravity
-}