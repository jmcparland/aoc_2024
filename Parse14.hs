{-# LANGUAGE OverloadedStrings #-}

module Parse14 where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

type Robot = [[Int]]

-- Parser for an integer
integer :: Parser Int
integer = do
    sign <- optional (char '-')
    num <- some digitChar
    return $ case sign of
        Just _ -> negate (read num)
        Nothing -> read num

-- Parser for a single point
pointParser :: Parser Robot
pointParser = do
    void $ char 'p' >> char '='
    x <- integer
    void $ char ','
    y <- integer
    void $ space
    void $ char 'v' >> char '='
    vx <- integer
    void $ char ','
    vy <- integer
    return $ [[x, y], [vx, vy]]

-- Parser for the entire input
pointsParser :: Parser [Robot]
pointsParser = pointParser `sepEndBy` newline

-- Function to parse the input text
parsePoints :: Text -> Either (ParseErrorBundle Text Void) [Robot]
parsePoints = parse pointsParser ""

getPoints :: String -> [Robot]
getPoints input = case parsePoints (T.pack input) of
    Left err -> error $ errorBundlePretty err
    Right points -> points
