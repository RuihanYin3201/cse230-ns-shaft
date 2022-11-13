
{-# LANGUAGE TemplateHaskell #-}
module Shaft (
    initGame,
    Game(..),
    life,
    score,
    curPlatforms,
    width,
    height,
    Coord,
    nextState
) where

import           Control.Lens              hiding ((:<), (:>), (<|), (|>))
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Linear.V3                 (V3 (..), _x, _y)
import           System.Random             (Random (..), RandomGen, newStdGen)

height, width :: Int
height = 40
width = 20

type Coord = V3 Int

data Game = Game {
    _life         :: Int,
    _score        :: Int,
    -- | coordinates of current platforms on the screen
    _curPlatforms :: [Coord],
    _allPlatforms :: [Coord],
    -- | 0: normal platform; 1: trap
    _traps        :: [Int],
    _dead         :: Bool
} deriving (Show)

makeLenses ''Game

initGame :: IO Game
initGame = do
  ap <- randomRs (V3 2 0 0, V3 (width - 3) 0 0) <$> newStdGen
  ts <- bernoulli 0.7 <$> newStdGen
  let
    g = Game {
    _life = 10,
    _score = 0,
    _curPlatforms = [V3 (width`div`2) 0 0],
    _allPlatforms = ap,
    _traps = ts,
    _dead = False
    }
  return g

nextState :: Game -> Game
nextState g = flip execState g.runMaybeT $ do
  MaybeT $ Just <$> modify move

move :: Game -> Game
move g@Game {_curPlatforms = ps, _allPlatforms = ap, _traps = ts} =
  if length ps == 15
    then do
      g & curPlatforms .~ (nextPlatform : map goUp (init ps))
        & allPlatforms .~ tail ap
        & traps .~ tail ts
    else do
      g & curPlatforms .~ (nextPlatform : map goUp ps)
        & allPlatforms .~ tail ap
        & traps .~ tail ts
  where
    hp = head ap
    nextPlatform = V3 (hp^._x) (hp^._y) (head ts)

-- | helper functions
goUp :: Coord -> Coord
goUp c = c & _y +~ 5

bernoulli :: RandomGen g => Float -> g -> [Int]
bernoulli p = map (\num -> if num > p then 1 else 0) . randoms
