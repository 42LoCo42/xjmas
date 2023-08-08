module XJmas where

import Parser
import Data.Char
import Control.Applicative

data Label = Direct String
           | Offset String Int
  deriving (Show)

data Arg = Label Label
         | Num Int
  deriving (Show)

data Line = Raw (Maybe String) Int
          | Asm (Maybe String) String [Arg]
          | Txt String
  deriving (Show)

nameP :: Parser String
nameP = spanP (\c -> isLetter c || c == '_')

sepP :: Parser String
sepP  = spanP isSeparator

potentially :: Parser a -> Parser (Maybe a)
potentially p = Just <$> p <|> pure Nothing

signedNumP :: Parser Int
signedNumP = charP '+' *> numsP <|>
             charP '-' *> ((*(-1)) <$> numsP)

labelDefP :: Parser String
labelDefP = potentially sepP *>
            nameP <* charP ':'
            <* potentially ws

labelUseP :: Parser Label
labelUseP = Offset <$> nameP <*> signedNumP <|>
            Offset "" <$> signedNumP <|>
            Direct <$> nameP

argP :: Parser Arg
argP = sepP *> (Label <$> labelUseP <|>
                Num   <$> numsP)

lineP :: Parser Line
lineP = Raw <$> potentially labelDefP <*> numsP <|>
        Asm <$> potentially labelDefP <*> nameP <*> many argP <|>
        Txt <$> spanP (/= '\n') <|>
        Txt <$> pure ""

programP :: Parser [Line]
programP = (:) <$> lineP <*> many (charP '\n' *> lineP)
