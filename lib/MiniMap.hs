module MiniMap
  ( MiniMapProjection,
    miniMapProjectionFor,
    miniMapSize,
    projectCell,
    unprojectCell,
  )
where

import Board (Board, Cell, liveCells)
import qualified Data.Set as Set

maxMiniMapWidth :: Int
maxMiniMapWidth = 28

maxMiniMapHeight :: Int
maxMiniMapHeight = 14

data MiniMapProjection = MiniMapProjection
  { projectionMin :: Cell,
    projectionSpan :: Cell,
    projectionSize :: (Int, Int)
  }

miniMapProjectionFor :: (Int, Int) -> Board -> Cell -> MiniMapProjection
miniMapProjectionFor viewportSize board viewport =
  MiniMapProjection
    { projectionMin = (minX, minY),
      projectionSpan = (spanX, spanY),
      projectionSize = sizeFor
    }
  where
    boardCells = Set.toList (liveCells board)
    ((minX, minY), (spanX, spanY)) = boundsFor viewportSize boardCells viewport
    sizeFor
      | Set.null (liveCells board) = (1, 1)
      | otherwise = fitSize spanX spanY

miniMapSize :: MiniMapProjection -> (Int, Int)
miniMapSize = projectionSize

projectCell :: MiniMapProjection -> Cell -> Cell
projectCell projection (x, y) =
  ( scaleCoordinate x minX spanX targetWidth,
    scaleCoordinate y minY spanY targetHeight
  )
  where
    (minX, minY) = projectionMin projection
    (spanX, spanY) = projectionSpan projection
    (targetWidth, targetHeight) = projectionSize projection

unprojectCell :: MiniMapProjection -> Cell -> Cell
unprojectCell projection (miniMapX, miniMapY) =
  ( unscaleCoordinate miniMapX minX spanX targetWidth,
    unscaleCoordinate miniMapY minY spanY targetHeight
  )
  where
    (minX, minY) = projectionMin projection
    (spanX, spanY) = projectionSpan projection
    (targetWidth, targetHeight) = projectionSize projection

boundsFor :: (Int, Int) -> [Cell] -> Cell -> (Cell, Cell)
boundsFor (viewportWidth, viewportHeight) boardCells (viewportX, viewportY) =
  ((minX, minY), (spanX, spanY))
  where
    xs = viewportX : map fst boardCells
    ys = viewportY : map snd boardCells
    maxXs = (viewportX + viewportWidth - 1) : xs
    maxYs = (viewportY + viewportHeight - 1) : ys
    minX = minimum xs
    minY = minimum ys
    maxX = maximum maxXs
    maxY = maximum maxYs
    spanX = max 1 (maxX - minX + 1)
    spanY = max 1 (maxY - minY + 1)

scaleCoordinate :: Int -> Int -> Int -> Int -> Int
scaleCoordinate value minValue spanValue targetSize =
  ((value - minValue) * max 0 (targetSize - 1)) `div` spanValue

unscaleCoordinate :: Int -> Int -> Int -> Int -> Int
unscaleCoordinate value minValue spanValue targetSize
  | targetSize <= 1 = minValue
  | otherwise =
      minValue + (value * spanValue) `div` max 1 (targetSize - 1)

fitSize :: Int -> Int -> (Int, Int)
fitSize spanX spanY
  | spanX * maxMiniMapHeight >= spanY * maxMiniMapWidth =
      (maxMiniMapWidth, max 1 (ceilingDiv (spanY * maxMiniMapWidth) spanX))
  | otherwise =
      (max 1 (ceilingDiv (spanX * maxMiniMapHeight) spanY), maxMiniMapHeight)

ceilingDiv :: Int -> Int -> Int
ceilingDiv numerator denominator =
  (numerator + denominator - 1) `div` denominator
