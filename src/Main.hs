module Main (main) where

import           Brick.AttrMap                  ( attrMap )
import           Brick.Main                     ( App(..)
                                                , continue
                                                , defaultMain
                                                , halt
                                                )
import           Brick.Types                    ( BrickEvent(VtyEvent)
                                                , Widget
                                                )
import           Brick.Widgets.Core             ( hBox
                                                , vBox
                                                )

import Graphics.Vty as V
import Brick.Widgets.Border as B
import Brick.Widgets.Border.Style as S
import Brick.Widgets.Center as C

import Graphics.Vty as V

import Mandelbrot

type Name = ()

data Pixel = Low | Med | High

height = 20
width = 20
magnification = 2.0
iterations = 100
threshold = 80
spacing = 5



app :: App View e Name
app = App { appDraw = drawApp,
            appChooseCursor = neverShowCursor,
            appHandleEvent = handleEvent,
            appStartEvent = return,
            appAttrMap = const colorMap}

main :: IO ()
main = initView >>= defaultMain app
  
handleEvent :: View -> BrickEvent Name e -> EventM Name View ()
handleEvent v (VtyEvent (V.EvKey V.KUp []))         = continue (move North spacing v)
handleEvent v (VtyEvent (V.EvKey V.KDown []))       = continue (move South spacing v)
handleEvent v (VtyEvent (V.EvKey V.KRight []))      = continue (move East spacing v)
handleEvent v (VtyEvent (V.EvKey V.KLeft []))       = continue (move West spacing v)
handleEvent v (VtyEvent (V.EvKey (V.KChar 'z') [])) = continue (zoomView magnification v)
handleEvent v (VtyEvent (V.EvKey (V.KChar 'z') [])) = continue (zoomView (1/magnification) v)
handleEvent v (VtyEvent (V.EvKey V.KEsc []))        = halt v
handleEvent v _                                     = continue v

drawApp :: View -> [Widget Name]
drawApp v = [C.center $ drawView v]

drawView :: View -> Widget Name
drawView v = withBorderStyle S.unicodeBold
    $ vBox rows
    where
        rows  = [hBox $ row r | r <- [height-1, height-2..0]]
        row r = [drawPixel (computeValue r c) | c <- [0..width-1]]
        computeValue i j
            | (iterateVal i j 0 0 0 iterations) == iterations  = High
            | (iterateVal i j 0 0 0 iterations) > threshold    = Med
            | otherwise                                        = Low

drawPixel :: Pixel -> Widget Name
drawPixel Low  = withAttr lowCol space
drawPixel Med  = withAttr medCol space
drawPixel High = withAttr highCol space

lowCol, medCol, highCol :: AttrName
lowCol  = "lowCol"
medCol  = "medCol"
highCol = "highCol"

space :: Widget Name
space = str " "

colorMap :: AttrMap
colorMap = attrMap V.defAttr
    [ (highCol, V.brightYellow `on` V.brightYellow),
      (medCol, V.yellow `on` V.yellow)]
