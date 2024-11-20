module Planets where
import Graphics.Gloss

data Planet = Planet { radius :: Float, pColor :: Color, xCoor :: Float, yCoor :: Float }

planetToPicture :: Planet -> Picture
planetToPicture p = Color (pColor p) $ ThickCircle 0 (radius p)

planetsToPicture :: [Planet] -> Picture
planetsToPicture ps = Pictures [ planetToPicture p | p <- ps ]