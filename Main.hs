module Main where

import XJmas
import Asm

import Parser (runParser)
import System.Environment (getArgs)

main = do
  args <- getArgs
  if length args /= 2 then
    error "Invalid arguments\nSpecify in and out file!"
  else do
    text <- (readFile $ args !! 0)
    let program = maybe [] id $ snd <$> runParser programP text
        zipped  = zipWithAddr 0 program
        output  = unlines $ map (assemble zipped) zipped
    writeFile (args !! 1) output
    print zipped
