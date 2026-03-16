module TerminalRender
  ( miniMapSizeFor,
    miniMapTargetCellFor,
    renderLayout,
  )
where

import Board (Board, Cell, isAlive, liveCells)
import qualified Data.Set as Set

maxMiniMapWidth :: Int
maxMiniMapWidth = 28

maxMiniMapHeight :: Int
maxMiniMapHeight = 14

edgeHintDistance :: Int
edgeHintDistance = 100

renderLayout :: (Int, Int) -> Board -> Cell -> Cell -> Maybe Cell -> String
renderLayout viewportSize board viewport cursorPosition maybeJumpCursor =
  unlines $
    zipWith
      (\boardRow miniMapRow -> boardRow ++ "    " ++ miniMapRow)
      paddedBoardLines
      paddedMiniMapLines
  where
    boardLines = renderBoardLines viewportSize board viewport cursorPosition
    miniMapLines = "Mini-map:" : renderMiniMapLines viewportSize board viewport maybeJumpCursor
    totalRows = max (length boardLines) (length miniMapLines)
    paddedBoardLines = padLines totalRows boardLines
    paddedMiniMapLines = padLines totalRows miniMapLines

viewportCells :: (Int, Int) -> Cell -> [Cell]
viewportCells (viewportWidth, viewportHeight) (viewportX, viewportY) =
  [ (x, y)
  | x <- [viewportX .. viewportX + viewportWidth - 1],
    y <- [viewportY .. viewportY + viewportHeight - 1]
  ]

renderCell :: Board -> Cell -> Cell -> Cell -> String
renderCell board viewport cursorPosition cell
  | (originX + x, originY + y) == cursorPosition = highlight contents
  | otherwise = contents
  where
    contents
      | isAlive board (originX + x, originY + y) = liveCell "██"
      | otherwise = "  "
    (originX, originY) = viewport
    (x, y) = cell

liveCell :: String -> String
liveCell contents = "\ESC[38;5;72m" ++ contents ++ "\ESC[0m"

highlight :: String -> String
highlight contents = "\ESC[7m" ++ contents ++ "\ESC[0m"

highlightMiniMap :: String -> String
highlightMiniMap contents = "\ESC[48;5;238m" ++ contents ++ "\ESC[0m"

highlightJumpCursor :: String -> String
highlightJumpCursor contents = "\ESC[7m" ++ contents ++ "\ESC[0m"

renderBoardLines :: (Int, Int) -> Board -> Cell -> Cell -> [String]
renderBoardLines (viewportWidth, viewportHeight) board viewport cursorPosition =
  [topBorder]
    ++ [showRow y | y <- [0 .. viewportHeight - 1]]
    ++ [bottomBorder]
  where
    (viewportX, viewportY) = viewport
    topBorder =
      "+"
        ++ concat
          [ borderSegment (hasAliveCellAlong (aboveCells x))
          | x <- [0 .. viewportWidth - 1]
          ]
        ++ "+"
    bottomBorder =
      "+"
        ++ concat
          [ borderSegment (hasAliveCellAlong (belowCells x))
          | x <- [0 .. viewportWidth - 1]
          ]
        ++ "+"
    showRow y =
      leftBorder y
        ++ concat [renderCell board viewport cursorPosition (x, y) | x <- [0 .. viewportWidth - 1]]
        ++ rightBorder y
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
    borderSegment shouldHighlight
      | shouldHighlight = liveCell "--"
      | otherwise = "--"
    edgeMarker shouldHighlight
      | shouldHighlight = liveCell "|"
      | otherwise = "|"

renderMiniMapLines :: (Int, Int) -> Board -> Cell -> Maybe Cell -> [String]
renderMiniMapLines viewportSize board viewport maybeJumpCursor
  | Set.null (liveCells board) = ["(empty)"]
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
      concat
        [ showMiniMapCell
            x
            y
            scaledCells
            scaledViewportCells
            maybeJumpCursor
        | x <- [0 .. miniMapWidth - 1]
        ]

showMiniMapCell :: Int -> Int -> Set.Set Cell -> Set.Set Cell -> Maybe Cell -> String
showMiniMapCell x y scaledCells scaledViewportCells maybeJumpCursor
  | Just (x, y) == maybeJumpCursor = highlightJumpCursor contents
  | (x, y) `Set.member` scaledViewportCells = highlightMiniMap contents
  | otherwise = contents
  where
    contents
      | (x, y) `Set.member` scaledCells = liveCell "#"
      | otherwise = "."

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

padLines :: Int -> [String] -> [String]
padLines targetLength rows =
  rows ++ replicate (targetLength - length rows) ""
