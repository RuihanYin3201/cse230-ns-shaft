{-# LANGUAGE TemplateHaskell #-}
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
    curPlatformPlayerOn,
    onTrap
) where

import           Control.Lens              hiding ((:<), (:>), (<|), (|>))
import           Control.Applicative       ((<|>))
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Linear.V3                 (V3 (..), _x, _y)
import           System.Random             (Random (..), RandomGen, newStdGen)

import Data.List (findIndex)
import Data.Maybe (fromMaybe)

import           Control.Monad             (guard)

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
    _myPlayer :: Coord,
    -- | 0: normal platform; 1: trap
    _traps        :: [Int],
    _dead         :: Bool,
    _onPlatform   :: Bool,
    _curPlatformPlayerOn  :: Coord,
    _onTrap       :: Bool
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
    _myPlayer = V3 (width`div`2) 20 0,
    _allPlatforms = ap,
    _traps = ts,
    _dead = False,
    _onPlatform = False,
    _curPlatformPlayerOn = V3 (width`div`2) 20 0,
    _onTrap = False
    }
  return g

nextState :: Game -> Game
nextState g = flip execState g.runMaybeT $ do
  MaybeT $ guard . not <$> use dead

  (MaybeT $ Just <$> modify move)

  -- die <|> onplatform <|> fall
  
  die <|> (MaybeT $ Just <$> modify movePlayer)

dieCond :: Game -> Bool
dieCond Game {_myPlayer = p, _life = life, _dead = d}
  | d == True = True
  | life < 0 = True
  | (p^._y) < 0 = True
  | (p^._y) > 40 = True
  | otherwise = False

die :: MaybeT (State Game) ()
die = do
  MaybeT . fmap guard $ dieCond <$> get
  MaybeT . fmap Just $ dead .= True

playerMoveLeft :: Game -> Game
playerMoveLeft g@Game {_myPlayer = p} = g & myPlayer .~ nextPos
  where nextPos = p & _x -~ 1

playerMoveRight :: Game -> Game
playerMoveRight g@Game {_myPlayer = p} = g & myPlayer .~ nextPos
  where nextPos = p & _x +~ 1

  -- TODO
  -- die <|> onPlatfrom <|> fall

movePlayer :: Game -> Game
movePlayer g@Game {_myPlayer = p, _onPlatform = op, _curPlatforms = cp, _curPlatformPlayerOn = cppo, _traps = tps, _score = s, _life = l} = 
  if pos > -1 
    -- position = -1 (default),  player not on any platform, free fall
    then do
      g & myPlayer .~ (p & _y .~ ((cp!!pos)^._y + 1))
        & onPlatform .~ (pos > -1)
        & curPlatformPlayerOn .~ (cp!!pos)
        & onPlatform .~ (tps!!pos == 0)
        & onTrap .~ ontrap

  else do 
    -- position != -1, meaning player on some platform from list, player go up with the same speed as other platforms
      g & myPlayer .~ (p & _y -~ 1)
        & onPlatform .~ False
        & onTrap .~ False
  where
    pos = fromMaybe (-1) $ findIndex (aroundPlatform p) (cp)
    ontrap = (tps!!pos) == 1

updateScore :: Int->Bool->Int 
updateScore oldScore True = oldScore + 1
updateScore oldScore False = oldScore

updateLife :: Int->Bool->Int 
updateLife oldLife True = oldLife - 1
updateLife oldLife False = oldLife
    



aroundPlatform :: V3 Int -> Coord -> Bool
aroundPlatform c p = c `elem` [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15]
  where
    y = p ^. _y 
    y1 = p ^._y - 1
    y2 = p^._y + 1
    c1 = V3 (p^._x-2) y 0
    c2 = V3 (p^._x-1) y 0
    c3 = V3 (p^._x) y 0
    c4 = V3 (p^._x+1) y 0
    c5 = V3 (p^._x+2) y 0
    c6 = V3 (p^._x-2) y1 0
    c7 = V3 (p^._x-1) y1 0
    c8 = V3 (p^._x) y1 0
    c9 = V3 (p^._x+1) y1 0
    c10 = V3 (p^._x+2) y1 0
    c11 = V3 (p^._x-2) y2 0
    c12 = V3 (p^._x-1) y2 0
    c13 = V3 (p^._x) y2 0
    c14 = V3 (p^._x+1) y2 0
    c15 = V3 (p^._x+2) y2 0




 
  


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
goUp c = c & _y +~ 2

bernoulli :: RandomGen g => Float -> g -> [Int]
bernoulli p = map (\num -> if num > p then 1 else 0) . randoms
