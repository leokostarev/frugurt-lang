module Main
  ( main
  ) where

import Control.Monad (unless, when)
import Data.Either (fromRight, isLeft)
import Data.List (intercalate)
import Debugize (toDbgStrStmt)
import Jsonize (toJsonStmt, toString)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (parse)
import Tokenize (fruTokenize)
import Treeanize (toAst)


possibleFlags :: [String]
possibleFlags = ["--debug"]


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

  let debugFlag = "--debug" `elem` flags

  raw <- readFile name

  -- tokens --
  let toksOrErr = parse fruTokenize name raw
  when (isLeft toksOrErr) $ do
    when debugFlag $ do
      putStrLn "---------- ERROR WHILE TOKENIZING ----------"
      print toksOrErr
    unless debugFlag $ do
      putStrLn ("{\"error\": \"tokenizing\", \"message\": " ++ show toksOrErr ++ "}")
    exitFailure

  let toks = fromRight undefined toksOrErr

  when debugFlag $ do
    putStrLn "---------- TOKENS ----------"
    putStrLn (intercalate "\n" $ map (("| " ++) . show) toks)

  -- ast --
  let astOrErr = parse toAst name toks
  when (isLeft astOrErr) $ do
    when debugFlag $ do
      putStrLn "---------- ERROR WHILE TREEANIZING ----------"
      print astOrErr
    unless debugFlag $ do
      putStrLn ("{\"error\": \"treeanizing\", \"message\": \"" ++ show astOrErr ++ "\"}")
    exitFailure

  let ast = fromRight undefined astOrErr
  when debugFlag $ do
    putStrLn "---------- AST ------------"
    putStrLn (toDbgStrStmt 0 ast)

  unless debugFlag $ do
    putStrLn (toString $ toJsonStmt ast)
