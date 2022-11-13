{-# LANGUAGE DataKinds #-}
module Main (main) where

import           Brick
import           Brick.BChan                (newBChan, writeBChan)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens               hiding ((:<), (:>), Empty, (<|),
                                             (|>))
import           Control.Monad              (forever, void)
import qualified Graphics.Vty               as V
import           Linear.V2                  (V2 (..), _x, _y)
import           Shaft

-- Custom ticker event
data Tick = Tick

-- App
app :: App Game Tick ()
app = App {
  appDraw = drawUI,
  appChooseCursor = neverShowCursor,
  appHandleEvent = handleEvent,
  appStartEvent = return,
  appAttrMap = const theMap
  }

handleEvent :: Game -> BrickEvent () Tick -> EventM () (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ nextState g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

main :: IO ()
main = do
  -- Ticker
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 1000000 -- decides how fast your game moves

  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- draw
drawUI :: Game -> [Widget ()]
drawUI g = [C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: Game -> Widget ()
drawStats g = hLimit 20 $ vBox [drawScore (g ^. score), padTop (Pad 4) $ drawLife $ g ^. life]

drawScore :: Int -> Widget ()
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawLife :: Int -> Widget ()
drawLife n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "life")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGrid :: Game -> Widget ()
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "ns-shaft")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | isPlatfrom c g      = Platform
      | otherwise           = Empty

isPlatfrom :: Coord -> Game -> Bool
isPlatfrom c g = any (inPlatfrom c) (g ^. curPlatforms)

inPlatfrom :: Coord -> Coord -> Bool
inPlatfrom c p = c `elem` [c1, c2, c3, c4, c5]
  where
    y = p ^. _y
    c1 = V2 (p^._x-2) y
    c2 = V2 (p^._x-1) y
    c3 = p
    c4 = V2 (p^._x+1) y
    c5 = V2 (p^._x+2) y

-- Cell
data Cell = Platform | Trap | Empty

drawCell :: Cell -> Widget ()
drawCell Platform = withAttr platformAttr cellWidget
drawCell Trap    = withAttr trapAttr cellWidget
drawCell Empty    = withAttr emptyAttr cellWidget

cellWidget :: Widget ()
cellWidget = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr [(platformAttr, V.black `on` V.black), (trapAttr, V.red `on` V.red)]

platformAttr, emptyAttr, trapAttr :: AttrName
platformAttr = attrName "platformAttr"
trapAttr = attrName "trapAttr"
emptyAttr = attrName "emptyAttr"
