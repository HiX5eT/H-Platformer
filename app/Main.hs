-- | Gökyüzü Adaları - Haskell ile yazılmış 3B platform oyunu.
module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Game.Update (initialWorld, handleEvent, updateWorld)
import Game.Render (renderWorld)

main :: IO ()
main = playIO
  (InWindow "Gokyuzu Adalari - 3D Platformer" (1280, 720) (60, 40))
  (makeColor 0.35 0.62 0.90 1)
  60
  initialWorld
  renderWorld
  handleEvent
  updateWorld
