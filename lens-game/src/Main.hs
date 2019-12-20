{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad
import Data.Map

main :: IO ()
main = do
  putStrLn "hello world"

---------------------------------------------------------------

data Game = Game
  {
    _score :: Int,
    _units :: [Unit],
    _boss :: Unit
  } deriving (Show)

data Unit = Unit
  {
    _health :: Int,
    _position :: Point
  } deriving (Show)

data Point = Point
  {
    _x :: Double,
    _y :: Double
  } deriving (Show)

------------------------------------------------------------------

score :: Lens' Game Int
score = lens _score (\game v -> game {_score = v})

units :: Lens' Game [Unit]
units = lens _units (\game v -> game {_units = v})

boss :: Lens' Game Unit
boss = lens _boss (\game v -> game {_boss = v})

health :: Lens' Unit Int
health = lens _health (\unit v -> unit {_health = v})

position :: Lens' Unit Point
position = lens _position (\unit v -> unit {_position = v})

x :: Lens' Point Double
x = lens _x (\point v -> point {_x = v})

y :: Lens' Point Double
y = lens _y (\point v -> point {_y = v})

------------------------------------------------------------------

-- makeLenses ''Game
-- makeLenses ''Unit
-- makeLenses ''Point

------------------------------------------------------------------

initialState :: Game
initialState = Game
  {
    _score = 0,
    _units = 
      [
        Unit
          {
            _health = 10,
            _position = Point {_x = 3.5, _y = 7.0}
          },
        Unit
          {
            _health = 15,
            _position = Point {_x = 1.0, _y = 1.0}
          },
        Unit
          {
            _health = 8,
            _position = Point {_x = 0.0, _y = 2.1}
          }
      ],
    _boss = Unit
      {
        _health = 100,
        _position = Point {_x = 0.0, _y = 0.0}
      }
  }

------------------------------------------------------------------

strike :: StateT Game IO ()
strike = do
  lift $ putStrLn "*shink*"
  -- boss . health -= 10
  bossHP -= 10

bossHP :: Lens' Game Int
bossHP = boss . health

------------------------------------------------------------------

fireBreath' :: StateT Game IO ()
fireBreath' = do
  lift $ putStrLn "*rawr*"
  -- units . traversed . health -= 3
  partyHP -= 3

partyHP :: Traversal' Game Int
partyHP = units . traversed . health

------------------------------------------------------------------

around :: (Choice p, Applicative f) => Point -> Double -> Optic' p f Unit Unit
around center radius = filtered (\unit -> (unit ^. position . x - center ^. x) ^ 2 + (unit ^. position . y - center ^. y) ^ 2 < radius ^ 2)

fireBreath :: Point -> StateT Game IO ()
fireBreath target = do
  lift $ putStrLn "*rawr*"
  units . traversed . (around target 1.0) . health -= 3

------------------------------------------------------------------

retreat :: StateT Game IO ()
retreat = do
  lift $ putStrLn "Retreat!"
  -- zoom (units . traversed . position) $ do
  zoom partyLoc $ do
    x += 10
    y += 10

partyLoc :: Traversal' Game Point
partyLoc = units . traversed . position

------------------------------------------------------------------

battle :: StateT Game IO ()
battle = do
  -- Charge!
  forM_ ["Take that!", "and that!", "and that!"] $ \taunt -> do
    lift $ putStrLn taunt
    strike

  -- The dragon awakes!
  fireBreath (Point 0.5 1.5)

  replicateM_ 3 $ do
    -- The better part of valor
    retreat

    -- Boss chases them
    zoom (boss . position) $ do
      x += 10
      y += 10