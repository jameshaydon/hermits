module Main where

import           Protolude

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.IO.Simulate
import           System.Random

type Shell = Point

shell :: Picture
shell = Circle 50

data Crab = Crab
  { position  :: Point
  , direction :: Point
  , size      :: Int
  }

data Component =
    C Crab
  | Shell Point

tran :: Point -> Picture -> Picture
tran (x,y) = Translate x y

viewCrab :: Picture -> Crab -> Picture
viewCrab bmp Crab{position = (x,y)} =
  Scale 0.2 0.2 $ Translate x y bmp

viewShell :: Point -> Picture
viewShell p = tran p (Circle 50)

addPoints :: Point -> Point -> Point
addPoints (x,y) (x',y') = (x+x', y+y')

viewComponent :: Picture -> Component -> Picture
viewComponent _ (Shell p) = viewShell p
viewComponent bmp (C c)   = viewCrab bmp c

viewCs :: Picture -> [Component] -> IO Picture
viewCs bmp = pure . foldMap (viewComponent bmp)

shouldTurn :: IO Bool
shouldTurn = (== 0) <$> randomRIO (0 :: Int, 120)

newAngle :: IO Float
newAngle = randomRIO (0, 2 * pi)

updateCrab :: Crab -> IO Crab
updateCrab c = do
  t <- shouldTurn
  moveCrab <$> if t
    then do a <- newAngle
            pure $ c { direction = angleToDir a }
    else pure c
  where
    angleToDir a = (cos a, sin a)
    moveCrab x@Crab{..} = x { position = addPoints position direction }

updateC :: Component -> IO Component
updateC (C c) = C <$> updateCrab c
updateC s     = pure s

update :: ViewPort -> Float -> [Component] -> IO [Component]
update _ _ cs = traverse updateC cs

main :: IO ()
main = do
  crabBmp <- loadBMP "hermies.bmp"
  simulateIO
    (InWindow "Nice Window" (800, 800) (10, 10))
    white
    60
    (replicate 10 (C Crab{position=(0,0), direction=(1,0)}))
    (viewCs crabBmp)
    update
