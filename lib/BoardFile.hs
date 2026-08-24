{-# LANGUAGE OverloadedStrings #-}

module BoardFile
  ( loadBoard,
    parseBoard,
  )
where

import Algorithm (Algorithm (..), rulesFor)
import Board (Board, Cell, makeBoard)
import Data.Aeson (FromJSON (parseJSON), eitherDecode)
import Data.Aeson.Types (Parser, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import LifeLike (nextBoardWithRules)

data BoardPayload = BoardPayload
  { payloadAlgorithm :: Algorithm,
    payloadCells :: [Cell]
  }

instance FromJSON BoardPayload where
  parseJSON = withObject "BoardPayload" $ \object ->
    BoardPayload
      <$> (parseAlgorithm =<< object .: "algorithm")
      <*> object .: "cells"

parseAlgorithm :: Text.Text -> Parser Algorithm
parseAlgorithm name =
  case Text.toLower name of
    "conway" -> pure Conway
    "highlife" -> pure HighLife
    _ -> fail "Unsupported algorithm"

loadBoard :: FilePath -> IO (Either String Board)
loadBoard path = parseBoard <$> BL.readFile path

parseBoard :: BL.ByteString -> Either String Board
parseBoard input =
  case eitherDecode input of
    Left message -> Left ("Invalid JSON: " ++ message)
    Right payload ->
      let nextBoardFn = nextBoardWithRules (rulesFor (payloadAlgorithm payload))
       in Right $ makeBoard nextBoardFn (payloadCells payload)
