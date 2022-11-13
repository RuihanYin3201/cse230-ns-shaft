
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
import           Linear.V2                 (V2 (..), _y)
import           System.Random             (Random (..), RandomGen, newStdGen)

data Game = Game {
    _life         :: Int,
    _score        :: Int,
    -- | coordinates of current platforms on the screen
    _curPlatforms :: [Coord],
    _allPlatforms :: [Coord],
    _dead         :: Bool
} deriving (Show)

type Coord = V2 Int

makeLenses ''Game

height, width :: Int
height = 20
width = 20

bernoulli :: RandomGen g => Float -> g -> [Int]
bernoulli p = map (\num -> if num > p then 1 else 0) . randoms

initGame :: IO Game
initGame = do
  ap <- randomRs (V2 2 0, V2 (width - 3) 0) <$> newStdGen
  ts <- bernoulli 0.25 <$> newStdGen
  let g = Game {
    _life = 10,
    _score = 0,
    _curPlatforms = [head ap],
    _allPlatforms = tail ap,
    _dead = False
    }
  return g

nextState :: Game -> Game
nextState g = flip execState g.runMaybeT $ do
  MaybeT $ Just <$> modify move

move :: Game -> Game
move g@Game {_curPlatforms = ps, _allPlatforms = ap} =
  if length ps == 10
    then do
      g & curPlatforms .~ (nextPlatform : map goUp (init ps))
        & allPlatforms .~ tail ap
    else do
      g & curPlatforms .~ (nextPlatform : map goUp ps)
        & allPlatforms .~ tail ap
  where
    nextPlatform = head ap

goUp :: Coord -> Coord
goUp c = c & _y +~ 5
