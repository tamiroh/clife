{-# LANGUAGE OverloadedStrings #-}

-- | Parses the "Extended RLE" pattern format, as specified at
-- <https://golly.sourceforge.io/Help/formats.html#rle>. Only two-state
-- (b\/o) rules are supported; multi-state (Generations-style) patterns
-- are rejected.
module RleFile
  ( loadBoard,
    parseBoard,
  )
where

import Board (Board, Cell, makeBoard)
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import LifeLike (Rules (..), nextBoardWithRules)

loadBoard :: FilePath -> IO (Either String Board)
loadBoard path = parseBoard <$> TextIO.readFile path

parseBoard :: Text.Text -> Either String Board
parseBoard input =
  case contentLines of
    [] -> Left "Empty RLE file"
    (headerLine : bodyLines)
      | isHeaderLine headerLine -> do
          rules <- parseRules headerLine
          cells <- parseBody (Text.concat bodyLines)
          Right (makeBoard (nextBoardWithRules rules) cells)
      | otherwise -> Left "Missing RLE header line (expected a line starting with \"x \" or \"x=\")"
  where
    -- Per the spec, a comment line is a blank line or one starting with "#".
    contentLines = filter (not . isCommentOrBlank) (Text.lines input)
    isCommentOrBlank line = Text.null (Text.strip line) || Text.isPrefixOf "#" line
    isHeaderLine line = Text.isPrefixOf "x " line || Text.isPrefixOf "x=" line

parseRules :: Text.Text -> Either String Rules
parseRules headerLine =
  case lookup "rule" fields of
    Just ruleText -> parseRuleString ruleText
    Nothing -> parseRuleString "B3/S23"
  where
    fields =
      [ (Text.toLower (Text.strip key), Text.strip value)
      | field <- Text.splitOn "," headerLine,
        (key, value) <- headerField field
      ]
    headerField field =
      case Text.splitOn "=" field of
        [key, value] -> [(key, value)]
        _ -> []

parseRuleString :: Text.Text -> Either String Rules
parseRuleString ruleText =
  case Text.splitOn "/" (Text.filter (not . Char.isSpace) ruleText) of
    [first, second] -> do
      (birthCounts, surviveCounts) <- combine (classify first) (classify second)
      Right Rules {birthWhenNeighborsAre = birthCounts, surviveWhenNeighborsAre = surviveCounts}
    _ -> invalid
  where
    invalid = Left ("Invalid rule string: " ++ Text.unpack ruleText)
    combine (Just ('b', bs)) (Just ('s', ss)) = Right (bs, ss)
    combine (Just ('s', ss)) (Just ('b', bs)) = Right (bs, ss)
    combine _ _ = invalid
    classify part =
      case Text.uncons part of
        Just (tag, digits)
          | Char.toLower tag == 'b' -> Just ('b', digitsToInts digits)
          | Char.toLower tag == 's' -> Just ('s', digitsToInts digits)
        _ -> Nothing
    digitsToInts = map Char.digitToInt . Text.unpack . Text.filter Char.isDigit

parseBody :: Text.Text -> Either String [Cell]
parseBody = go 0 0 0 []
  where
    go :: Int -> Int -> Int -> [Cell] -> Text.Text -> Either String [Cell]
    go count x y acc remaining =
      case Text.uncons remaining of
        Nothing -> Right acc
        Just ('!', _) -> Right acc
        Just (c, rest)
          | Char.isDigit c -> go (count * 10 + Char.digitToInt c) x y acc rest
          | Char.isSpace c -> go count x y acc rest
          | c == 'b' -> go 0 (x + effectiveCount) y acc rest
          | c == 'o' -> go 0 (x + effectiveCount) y (newLiveCells ++ acc) rest
          | c == '$' -> go 0 0 (y + effectiveCount) acc rest
          | otherwise -> Left ("Unexpected character in RLE body: " ++ [c])
          where
            effectiveCount = if count == 0 then 1 else count
            newLiveCells = [(x + offset, y) | offset <- [0 .. effectiveCount - 1]]
