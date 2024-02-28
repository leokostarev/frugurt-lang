module Main
  ( main
  ) where

import Control.Monad (unless, when)
import Data.Either (fromRight, isLeft)
import Data.List (intercalate)
import Jsonize (toJsonStmt, toString)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (parse)
import Tokenize (fruTokenize)
import Treeanize (toAst)


possibleFlags :: [String]
possibleFlags = ["--detailed"]


main :: IO ()
main = do
  arguments <- getArgs
  when (null arguments) $ do
    print "No source files specified"
    exitFailure

  let name = head arguments
  let flags = tail arguments
  let unknownFlags = filter (`notElem` possibleFlags) flags

  unless (null unknownFlags) $ do
    print $ "Unknown flags" ++ intercalate ", " unknownFlags
    exitFailure

  let detailedFlag = "--detailed" `elem` flags

  raw <- readFile name

  -- tokens --
  let toksOrErr = parse fruTokenize name raw
  when (isLeft toksOrErr) $ do
    putStrLn "---------- ERROR WHILE TOKENIZING ----------"
    print toksOrErr
    exitFailure

  let toks = fromRight undefined toksOrErr

  when detailedFlag $ do
    putStrLn "---------- TOKENS ----------"
    putStrLn $ intercalate "\n" $ map (("| " ++) . show) toks

  -- ast --
  let astOrErr = parse toAst name toks
  when (isLeft astOrErr) $ do
    putStrLn "---------- ERROR WHILE TREEANIZING ----------"
    print astOrErr
    exitFailure

  let ast = fromRight undefined astOrErr
  when detailedFlag $ do
    putStrLn "---------- AST ------------"
    print ast

  let str = toString $ toJsonStmt ast
  when detailedFlag $ do
    putStrLn "---------- STRING ----------"

  putStrLn str
