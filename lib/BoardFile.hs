{-# LANGUAGE OverloadedStrings #-}

module BoardFile
  ( loadBoard,
    parseBoard
  )
where

import Data.Aeson (FromJSON (parseJSON), eitherDecode)
import Data.Aeson.Types (withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import GameOfLife (Board, Cell, makeBoard)

newtype BoardPayload = BoardPayload
  { payloadCells :: [Cell]
  }

instance FromJSON BoardPayload where
  parseJSON = withObject "BoardPayload" $ \object ->
    BoardPayload <$> object .: "cells"

loadBoard :: FilePath -> IO (Either String Board)
loadBoard path = parseBoard <$> BL.readFile path

parseBoard :: BL.ByteString -> Either String Board
parseBoard input =
  case eitherDecode input of
    Left message -> Left ("Invalid JSON: " ++ message)
    Right payload -> Right (makeBoard (payloadCells payload))
