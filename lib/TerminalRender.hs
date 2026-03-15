module TerminalRender
  ( viewportWidth,
    viewportHeight,
    miniMapSizeFor,
    miniMapTargetCellFor,
    renderLayout,
  )
where

import Board (Board, Cell, isAlive, liveCells)
import qualified Data.Set as Set

viewportWidth :: Int
viewportWidth = 40

viewportHeight :: Int
viewportHeight = 20

maxMiniMapWidth :: Int
maxMiniMapWidth = 28

maxMiniMapHeight :: Int
maxMiniMapHeight = 14

edgeHintDistance :: Int
edgeHintDistance = 6

renderLayout :: Board -> Cell -> Cell -> Maybe Cell -> String
renderLayout board viewport cursorPosition maybeJumpCursor =
  unlines $
    zipWith
      (\boardRow miniMapRow -> boardRow ++ "    " ++ miniMapRow)
      paddedBoardLines
      paddedMiniMapLines
  where
    boardLines = renderBoardLines board viewport cursorPosition
    miniMapLines = "Mini-map:" : renderMiniMapLines board viewport maybeJumpCursor
    totalRows = max (length boardLines) (length miniMapLines)
    paddedBoardLines = padLines totalRows boardLines
    paddedMiniMapLines = padLines totalRows miniMapLines

viewportCells :: Cell -> [Cell]
viewportCells (viewportX, viewportY) =
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

renderBoardLines :: Board -> Cell -> Cell -> [String]
renderBoardLines board viewport cursorPosition =
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

renderMiniMapLines :: Board -> Cell -> Maybe Cell -> [String]
renderMiniMapLines board viewport maybeJumpCursor
  | Set.null (liveCells board) = ["(empty)"]
  | otherwise = [showMiniMapRow y | y <- [0 .. miniMapHeight - 1]]
  where
    boardCells = Set.toList (liveCells board)
    ((minX, minY), (spanX, spanY)) = miniMapBounds boardCells viewport
    (miniMapWidth, miniMapHeight) = miniMapSizeFor board viewport
    scaleCell (x, y) =
      ( scaleCoordinate x minX spanX miniMapWidth,
        scaleCoordinate y minY spanY miniMapHeight
      )
    scaledCells = Set.fromList (map scaleCell boardCells)
    scaledViewportCells = Set.fromList (map scaleCell (viewportCells viewport))
    showMiniMapRow y =
      concat
        [ showMiniMapCell x y scaledCells scaledViewportCells
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

miniMapBounds :: [Cell] -> Cell -> (Cell, Cell)
miniMapBounds boardCells (viewportX, viewportY) =
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

miniMapSizeFor :: Board -> Cell -> (Int, Int)
miniMapSizeFor board viewport
  | Set.null (liveCells board) = (1, 1)
  | otherwise = miniMapSize spanX spanY
  where
    boardCells = Set.toList (liveCells board)
    (_, (spanX, spanY)) = miniMapBounds boardCells viewport

miniMapTargetCellFor :: Board -> Cell -> Cell -> Cell
miniMapTargetCellFor board viewport (miniMapX, miniMapY) =
  ( unscaleCoordinate miniMapX minX spanX miniMapWidth,
    unscaleCoordinate miniMapY minY spanY miniMapHeight
  )
  where
    boardCells = Set.toList (liveCells board)
    ((minX, minY), (spanX, spanY)) = miniMapBounds boardCells viewport
    (miniMapWidth, miniMapHeight) = miniMapSizeFor board viewport

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
