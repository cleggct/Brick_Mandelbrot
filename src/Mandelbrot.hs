module Mandelbrot where

iterateVal :: Float -> Float -> Float -> Float -> Int -> Int -> Int
iterateVal x0 y0 x y n lim = if ((x*x + y*y <= 4) && (n < lim))
                                then iterateVal x0 y0 (x*x - y*y + x0) (2*x*y + y0) (n+1) lim
                                else n
                        
data View = View
    { _centerX :: Int,
      _centerY :: Int,
      _zoomLvl   :: Float} deriving (Show)
     
data Direction
    = North
    | South
    | East
    | West
    deriving (Eq, Show)
     
move :: Direction -> Int -> View -> View
move North a (View {_centerX = x, _centerY = y, _zoomLvl = z}) = View { _centerX = x, _centerY = y - a, _zoomLvl = z}
move South a (View {_centerX = x, _centerY = y, _zoomLvl = z}) = View { _centerX = x, _centerY = y + a, _zoomLvl = z}
move West a (View {_centerX = x, _centerY = y, _zoomLvl = z}) = View { _centerX = x - a, _centerY = y, _zoomLvl = z}
move East a (View {_centerX = x, _centerY = y, _zoomLvl = z}) = View { _centerX = x + a, _centerY = y, _zoomLvl = z}

zoomView :: Float -> View -> View
zoomView z' (View {_centerX = x, _centerY = y, _zoomLvl = z}) = View { _centerX = x, _centerY = y, _zoomLvl = z*z' }

initView :: View
initView = View {_centerX = 0, _centerY = 0, _zoomLvl = 1.0}
