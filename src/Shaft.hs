{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Shaft (
    initGame,
    Game(..),
    life,
    dead,
    score,
    curPlatforms,
    myPlayer,
    width,
    height,
    Coord,
    nextState,
    playerMoveLeft,
    playerMoveRight,
    movePlayer,
    onTrap,
    curInt,
    interval,
    prop_genBernoulliList
) where

import           Control.Applicative       ((<|>))
import           Control.Lens              hiding ((:<), (:>), (<|), (|>))
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Linear.V3                 (V3 (..), _x, _y, _z)
import           System.Random             (Random (..), mkStdGen, newStdGen)

import           Data.List                 (findIndices)

import           Control.Concurrent.STM
import           Control.Monad             (guard)
import           Test.QuickCheck

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
    _myPlayer     :: Coord,
    -- | 0: normal platform; 1: trap
    _traps        :: [Int],
    _dead         :: Bool,
    _onPlatform   :: Bool,
    _onTrap       :: Bool,
    _tickCounter  :: Int,
    _interval     :: TVar Int,
    _curInt       :: Int
-- } deriving (Show)
}

makeLenses ''Game

initGame :: TVar Int -> IO Game
initGame tv = do
  ap <- randomRs (V3 2 0 0, V3 (width - 3) 0 0) <$> newStdGen
  let ts = bernoulli 0.7
  -- ts <- bernoulli 0.7 <$> newStdGen
  let
    g = Game {
    _life = 10,
    _score = 0,
    _curPlatforms = [V3 (width`div`2) 0 0],
    _myPlayer = V3 (width`div`2) 20 0,
    _allPlatforms = ap,
    _traps = ts,
    _dead = False,
    _onPlatform = False,
    _onTrap = False,
    _tickCounter = 1,
    _interval = tv,
    _curInt = 1000000
    }
  return g

nextState :: Game -> Game
nextState g = flip execState g.runMaybeT $ do
  MaybeT $ guard . not <$> use dead
  -- MaybeT $ Just <$> modify speedUp
  MaybeT $ Just <$> modify countTick
  MaybeT $ Just <$> modify move
  die <|> MaybeT (Just <$> modify movePlayer)

  -- die <|> onplatform <|> fall


dieCond :: Game -> Bool
dieCond Game {_myPlayer = p, _life = playerLife, _dead = d}
  | d == True = True
  | playerLife <= 0 = True
  | (p^._y) < 0 = True
  | (p^._y) > 40 = True
  | otherwise = False

die :: MaybeT (State Game) ()
die = do
  MaybeT . fmap guard $ dieCond <$> get
  MaybeT . fmap Just $ dead .= True

playerMoveLeft :: Game -> Game
playerMoveLeft g@Game {_myPlayer = p} = g & myPlayer .~ nextPos
  where nextPos = if   p ^. _x > 0
                  then p & _x -~ 1
                  else p

playerMoveRight :: Game -> Game
playerMoveRight g@Game {_myPlayer = p} = g & myPlayer .~ nextPos
  where nextPos = if   p ^. _x < 19
                  then p & _x +~ 1
                  else p

  -- TODO
  -- die <|> onPlatfrom <|> fall

movePlayer :: Game -> Game
movePlayer g@Game {_myPlayer = p, _onPlatform = onPlf, _curPlatforms = cp, _traps = tps, _score = s, _life = l, _onTrap = ot} =
  if pos > -1
    -- position != -1, meaning player on some platform from list, player go up with the same speed as other platforms
    then do
      g & myPlayer .~ (p & _y .~ ((cp!!pos)^._y + 1))
        & onPlatform .~ (pos > -1)
        & onPlatform .~ (tps!!pos == 0)
        & score .~ updateScore s ontrap onPlf -- score + 1 when previous not on platform, currently not on trap
        & life .~ updateLife s l ot ontrap onPlf -- life -1 when on trap, life + 1 when score reaches 10s. 
        & onTrap .~ ontrap
  else do
    -- position = -1 (default),  player not on any platform, free fall
      g & myPlayer .~ (p & _y -~ 1)
        & onPlatform .~ False
        & onTrap .~ False
  where
    -- pos = fromMaybe (-1) $ findIndex (aroundPlatform p) cp
    pos = if null idxs then -1 else last idxs
    idxs = findIndices (aroundPlatform p) cp
    ontrap = (cp!!pos) ^. _z == 1

updateScore :: Int->Bool->Bool-> Int
-- updateScore oldScore currentlyOnTrap previousOnPlatform
updateScore oldScore False False = oldScore + 1 -- if not on trap, prevOnPlatform = false, currently on platform, then score + 1
updateScore oldScore _ _         = oldScore -- else, don't change score



updateLife :: Int-> Int->Bool->Bool->Bool-> Int
-- updateLife oldLife previousOnTrap currentlyOnTrap
updateLife oldScore oldLife True True _ = oldLife
updateLife oldScore oldLife False True _ = oldLife - 2
updateLife oldScore oldLife _ False False 
  | oldScore `mod` 10 == 0      = if oldLife < 10 then oldLife + 1 else oldLife 
  | otherwise                   = oldLife
updateLife oldScore oldLife _ _ _ = oldLife


aroundPlatform :: V3 Int -> Coord -> Bool
aroundPlatform c p = c `elem` [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15]
  where
    y   = p ^. _y +1
    y1  = p ^._y
    y2  = p^._y - 1
    c1  = V3 (p^._x-2) y  0
    c2  = V3 (p^._x-1) y  0
    c3  = V3 (p^._x)   y  0
    c4  = V3 (p^._x+1) y  0
    c5  = V3 (p^._x+2) y  0
    c6  = V3 (p^._x-2) y1 0
    c7  = V3 (p^._x-1) y1 0
    c8  = V3 (p^._x)   y1 0
    c9  = V3 (p^._x+1) y1 0
    c10 = V3 (p^._x+2) y1 0
    c11 = V3 (p^._x-2) y2 0
    c12 = V3 (p^._x-1) y2 0
    c13 = V3 (p^._x)   y2 0
    c14 = V3 (p^._x+1) y2 0
    c15 = V3 (p^._x+2) y2 0

countTick :: Game -> Game
countTick g = g & tickCounter +~ 1

move :: Game -> Game
move g@Game {_curPlatforms = ps, _allPlatforms = ap, _traps = ts, _tickCounter = tc}
  | even tc = if length ps == 18
                  then do
                    g & curPlatforms .~ map goUp (init ps)
                  else do
                    g & curPlatforms .~ map goUp ps
  | otherwise =   if length ps == 18
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
goUp c = c & _y +~ 2

bernoulli :: Double -> [Int]
bernoulli p = map (\num -> if num > p then 1 else 0) . randoms $ mkStdGen 230


-- Properties
prop_genBernoulliList :: Property
prop_genBernoulliList = forAll genProbAndList (\(p, ls) -> abs (ratio ls - p) <= 0.01)
  where
    ratio ls = 1- (fromIntegral (sum ls) / 10000)

genProbAndList :: Gen (Double, [Int])
genProbAndList = do
  p <- chooseInt (1, 10)
  let prob = 1 / fromIntegral p
  let ls = take 10000 $ bernoulli prob
  return (prob, ls)
