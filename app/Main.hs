module Main where
import Graphics.Gloss

-- Main function to run the program
main :: IO ()
main = animate
    (InWindow "Moving Circle" (400, 400) (100, 100)) -- Window size and position
    white                                              -- Background color
    frame                                             -- Frame function

-- Function to generate each frame of the animation
frame :: Float -> Picture
frame time = Translate x 0 $ Color blue $ Circle 50
  where
    x = 100 * sin time -- X-coordinate changes with time
