{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.IO
import Data.ByteString (intercalate)
import System.Exit
placar :: [Int] -> Int
placar ([]) = 0
placar (r1 : []) = r1
placar (r1 : r2 : []) = r1 + r2
placar (r1 : r2 : r3 : []) = r1 + r2 + r3
placar (r1 : r2 : r3 : rs)
  | r1 == 10 = do
        r1 + r2 + r3 + placar(r2 : r3 : rs)

  | (r1 + r2) == 10 = do 
        r1 + r2 + r3 + placar(r3 : rs)
  | otherwise = do
        r1 + r2 + placar(r3 : rs)

-----------------------------------------------------
printPlacar :: [Int] -> IO()
printPlacar ([]) = putStr "|"

printPlacar (r1 : [])
  |r1 == 10 = do
      putStr ("X | ")
  | otherwise = do
      putStr (show r1 ++ " | ") 

printPlacar (r1 : r2 : [])
  | r1 == 10 = do
      putStr("X ")
      printPlacar(r2 : [])
  |(r1 + r2) == 10 = do
      putStr (show r1 ++ " / | ")
      
  |otherwise = do
      putStr  (show r1 ++ " " ++ show r2 ++ " | ")

  
printPlacar (r1 : r2 : r3 : [])
  | r1 == 10 = do
      putStr ("X ")
      printPlacar (r2 : r3 : [])

  | (r1 + r2) == 10 = do
      putStr (show r1 ++ " " ++ "/ ")
      printPlacar(r3 : [])
  | otherwise = do
      putStr (show r1 ++ " " ++ show r2 ++ " " ++ show r3 ++ " | ")

printPlacar (r1 : r2 : r3 : rs)
  |r1 == 10 = do
      putStr "X _ | "
      printPlacar (r2 : r3 : rs)
  |(r1 + r2) == 10 = do
      putStr (show r1 ++  " / | ")
      printPlacar(r3 : rs)
  |otherwise = do
      putStr $ show r1 ++ " " ++ show r2 ++ " | "
      printPlacar(r3 : rs)


strLToIntL :: [String] -> [Int]
strLToIntL = map read


main :: IO()
main = do
  input <- getLine
  let intList = map (read :: String -> Int) $ words input
  printPlacar intList
  print $ placar intList

