{-# LANGUAGE OverloadedStrings #-}

module Parse13 where

import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

data Case = Case
    { buttonA :: (Int, Int)
    , buttonB :: (Int, Int)
    , prize :: (Int, Int)
    }
    deriving (Show)

parseButton :: Text -> Parser (Int, Int)
parseButton label = do
    _ <- string label
    _ <- string " X+"
    x <- L.decimal
    _ <- string ", Y+"
    y <- L.decimal
    return (x, y)

parsePrize :: Parser (Int, Int)
parsePrize = do
    _ <- string "Prize: X="
    x <- L.decimal
    _ <- string ", Y="
    y <- L.decimal
    return (x, y)

parseCase :: Parser Case
parseCase = do
    buttonA <- parseButton "Button A:"
    _ <- newline
    buttonB <- parseButton "Button B:"
    _ <- newline
    prize <- parsePrize
    _ <- optional newline
    return Case{buttonA = buttonA, buttonB = buttonB, prize = prize}

parseCases :: Parser [Case]
parseCases = many (parseCase <* optional newline)

getCases :: String -> [Case]
getCases input = do
    let parsed = parse parseCases "" $ pack input
    case parsed of
        Left bundle -> error $ errorBundlePretty bundle
        Right cases -> cases
