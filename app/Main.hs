module Main where

import InterpreterBase

main :: IO ()
main = do
  text <- readFile filep
  let stmts = readStats text
  mapM_ putStrLn (map (show) stmts)
  run $ mconcat stmts

filep :: FilePath
filep = "./test/program.txt"

readStats :: String -> [Statement]
readStats s = map read listOfStatements
  where listOfStatements = lines s
