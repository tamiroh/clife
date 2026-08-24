module TerminalRender
  ( miniMapSizeFor,
    miniMapTargetCellFor,
    renderLayout,
  )
where

import Board (Board, Cell, isAlive, liveCells)
import Brick.Types (Widget)
import Brick.Widgets.Core (raw)
import qualified Data.Set as Set
import Graphics.Vty
  ( Attr,
    Color (Color240),
    Image,
    charFill,
    defAttr,
    horizCat,
    reverseVideo,
    string,
    vertCat,
    withBackColor,
    withForeColor,
    withStyle,
  )

maxMiniMapWidth :: Int
maxMiniMapWidth = 28

maxMiniMapHeight :: Int
maxMiniMapHeight = 14

edgeHintDistance :: Int
edgeHintDistance = 100

liveAttr :: Attr
liveAttr = defAttr `withForeColor` Color240 56

viewportBgAttr :: Attr
viewportBgAttr = defAttr `withBackColor` Color240 222

liveViewportBgAttr :: Attr
liveViewportBgAttr = defAttr `withForeColor` Color240 56 `withBackColor` Color240 222

reverseAttr :: Attr
reverseAttr = defAttr `withStyle` reverseVideo

liveReverseAttr :: Attr
liveReverseAttr = defAttr `withForeColor` Color240 56 `withStyle` reverseVideo

renderLayout :: (Int, Int) -> Board -> Cell -> Cell -> Maybe Cell -> Widget n
renderLayout viewportSize board viewport cursorPosition maybeJumpCursor =
  raw (horizCat [boardImage, gapImage, miniMapImage])
  where
    boardImage = renderBoardImage viewportSize board viewport cursorPosition
    miniMapImage =
      vertCat (string defAttr "Mini-map:" : renderMiniMapImages viewportSize board viewport maybeJumpCursor)
    gapImage = charFill defAttr ' ' (4 :: Int) (1 :: Int)

viewportCells :: (Int, Int) -> Cell -> [Cell]
viewportCells (viewportWidth, viewportHeight) (viewportX, viewportY) =
  [ (x, y)
  | x <- [viewportX .. viewportX + viewportWidth - 1],
    y <- [viewportY .. viewportY + viewportHeight - 1]
  ]

renderCellImage :: Board -> Cell -> Cell -> Cell -> Image
renderCellImage board viewport cursorPosition cell
  | (originX + x, originY + y) == cursorPosition = string (cursorAttrFor alive) contents
  | otherwise = string (attrFor alive) contents
  where
    (originX, originY) = viewport
    (x, y) = cell
    alive = isAlive board (originX + x, originY + y)
    contents = if alive then "██" else "  "

attrFor :: Bool -> Attr
attrFor alive = if alive then liveAttr else defAttr

cursorAttrFor :: Bool -> Attr
cursorAttrFor alive = if alive then liveReverseAttr else reverseAttr

renderBoardImage :: (Int, Int) -> Board -> Cell -> Cell -> Image
renderBoardImage (viewportWidth, viewportHeight) board viewport cursorPosition =
  vertCat ([topBorder] ++ [showRow y | y <- [0 .. viewportHeight - 1]] ++ [bottomBorder])
  where
    (viewportX, viewportY) = viewport
    topBorder =
      horizCat $
        [string defAttr "+"]
          ++ [borderSegment (hasAliveCellAlong (aboveCells x)) | x <- [0 .. viewportWidth - 1]]
          ++ [string defAttr "+"]
    bottomBorder =
      horizCat $
        [string defAttr "+"]
          ++ [borderSegment (hasAliveCellAlong (belowCells x)) | x <- [0 .. viewportWidth - 1]]
          ++ [string defAttr "+"]
    showRow y =
      horizCat $
        [leftBorder y]
          ++ [renderCellImage board viewport cursorPosition (x, y) | x <- [0 .. viewportWidth - 1]]
          ++ [rightBorder y]
    leftBorder y = edgeMarker (hasAliveCellAlong (leftCells y))
    rightBorder y = edgeMarker (hasAliveCellAlong (rightCells y))
    aboveCells x =
      [ (viewportX + x, viewportY - distance)
      | distance <- [1 .. edgeHintDistance]
      ]
    belowCells x =
      [ (viewportX + x, viewportY + viewportHeight + distance - 1)
      | distance <- [1 .. edgeHintDistance]
      ]
    leftCells y =
      [ (viewportX - distance, viewportY + y)
      | distance <- [1 .. edgeHintDistance]
      ]
    rightCells y =
      [ (viewportX + viewportWidth + distance - 1, viewportY + y)
      | distance <- [1 .. edgeHintDistance]
      ]
    hasAliveCellAlong = any (isAlive board)
    borderSegment shouldHighlight = string (attrFor shouldHighlight) "--"
    edgeMarker shouldHighlight = string (attrFor shouldHighlight) "|"

renderMiniMapImages :: (Int, Int) -> Board -> Cell -> Maybe Cell -> [Image]
renderMiniMapImages viewportSize board viewport maybeJumpCursor
  | Set.null (liveCells board) = [string defAttr "(empty)"]
  | otherwise = [showMiniMapRow y | y <- [0 .. miniMapHeight - 1]]
  where
    boardCells = Set.toList (liveCells board)
    ((minX, minY), (spanX, spanY)) = miniMapBounds viewportSize boardCells viewport
    (miniMapWidth, miniMapHeight) = miniMapSizeFor viewportSize board viewport
    scaleCell (x, y) =
      ( scaleCoordinate x minX spanX miniMapWidth,
        scaleCoordinate y minY spanY miniMapHeight
      )
    scaledCells = Set.fromList (map scaleCell boardCells)
    scaledViewportCells = Set.fromList (map scaleCell (viewportCells viewportSize viewport))
    showMiniMapRow y =
      horizCat
        [ showMiniMapCellImage
            x
            y
            scaledCells
            scaledViewportCells
            maybeJumpCursor
        | x <- [0 .. miniMapWidth - 1]
        ]

showMiniMapCellImage :: Int -> Int -> Set.Set Cell -> Set.Set Cell -> Maybe Cell -> Image
showMiniMapCellImage x y scaledCells scaledViewportCells maybeJumpCursor
  | Just (x, y) == maybeJumpCursor = string (cursorAttrFor alive) contents
  | (x, y) `Set.member` scaledViewportCells = string (viewportAttrFor alive) contents
  | otherwise = string (attrFor alive) contents
  where
    alive = (x, y) `Set.member` scaledCells
    contents = if alive then "#" else "."
    viewportAttrFor isAliveCell = if isAliveCell then liveViewportBgAttr else viewportBgAttr

miniMapBounds :: (Int, Int) -> [Cell] -> Cell -> (Cell, Cell)
miniMapBounds (viewportWidth, viewportHeight) boardCells (viewportX, viewportY) =
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

miniMapSizeFor :: (Int, Int) -> Board -> Cell -> (Int, Int)
miniMapSizeFor viewportSize board viewport
  | Set.null (liveCells board) = (1, 1)
  | otherwise = miniMapSize spanX spanY
  where
    boardCells = Set.toList (liveCells board)
    (_, (spanX, spanY)) = miniMapBounds viewportSize boardCells viewport

miniMapTargetCellFor :: (Int, Int) -> Board -> Cell -> Cell -> Cell
miniMapTargetCellFor viewportSize board viewport (miniMapX, miniMapY) =
  ( unscaleCoordinate miniMapX minX spanX miniMapWidth,
    unscaleCoordinate miniMapY minY spanY miniMapHeight
  )
  where
    boardCells = Set.toList (liveCells board)
    ((minX, minY), (spanX, spanY)) = miniMapBounds viewportSize boardCells viewport
    (miniMapWidth, miniMapHeight) = miniMapSizeFor viewportSize board viewport

miniMapSize :: Int -> Int -> (Int, Int)
miniMapSize spanX spanY
  | spanX * maxMiniMapHeight >= spanY * maxMiniMapWidth =
      (maxMiniMapWidth, max 1 (ceilingDiv (spanY * maxMiniMapWidth) spanX))
  | otherwise =
      (max 1 (ceilingDiv (spanX * maxMiniMapHeight) spanY), maxMiniMapHeight)

ceilingDiv :: Int -> Int -> Int
ceilingDiv numerator denominator =
  (numerator + denominator - 1) `div` denominator
