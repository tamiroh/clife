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

data BoardPayload = BoardPayload
  { payloadWidth :: Int,
    payloadHeight :: Int,
    payloadCells :: [Cell]
  }

instance FromJSON BoardPayload where
  parseJSON = withObject "BoardPayload" $ \object ->
    BoardPayload
      <$> object .: "width"
      <*> object .: "height"
      <*> object .: "cells"

loadBoard :: FilePath -> IO (Either String Board)
loadBoard path = parseBoard <$> BL.readFile path

parseBoard :: BL.ByteString -> Either String Board
parseBoard input =
  case eitherDecode input of
    Left message -> Left ("Invalid JSON: " ++ message)
    Right payload ->
      case makeBoard (payloadWidth payload) (payloadHeight payload) (payloadCells payload) of
        Just board -> Right board
        Nothing -> Left "The board contains out-of-bounds cells or invalid dimensions."
