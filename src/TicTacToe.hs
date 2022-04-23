module TicTacToe (
    run
) where

import Data.List (sort, (\\))
import Control.Monad ( when )
import Text.Read (readMaybe )
import System.IO (hFlush, stdout)

type P = Int -- 0..8 - position
type C = Int -- -1, 1, 0 - cost
type M = Int -- 1, 2 - min | max
newtype Track = Track {list :: [P] }  deriving Show 

clrscr = "\27[0;0H\27[J"
origin =  "\27[0;0H"
red = "\27[31m"
norm = "\27[m"
gray = "\27[1m\27[30m"

drawBoard :: Track -> String -> IO ()
drawBoard track message = do  
   putStr $ clrscr ++ gray ++ "0  1  2\n3  4  5\n6  7  8" ++ origin ++ norm
   mapM_ drawChar (zip (reverse (list track)) "xoxoxoxox")
   putStr "\27[4;1f"    -- line 4; pos 1
   putStrLn message
      where
         drawChar (pos, c) = let (x, y) = divMod pos 3
            in putStr $ "\27[" ++ show (x+1) ++ ";" ++ show (3*y+1) ++"f" ++ [c]
   
-- https://ru.wikipedia.org/wiki/%D0%A3%D0%BF%D1%80%D0%B0%D0%B2%D0%BB%D1%8F%D1%8E%D1%89%D0%B8%D0%B5_%D0%BF%D0%BE%D1%81%D0%BB%D0%B5%D0%B4%D0%BE%D0%B2%D0%B0%D1%82%D0%B5%D0%BB%D1%8C%D0%BD%D0%BE%D1%81%D1%82%D0%B8_ANSI

(~) :: P -> Track -> Track
pos ~ track = Track $ pos : list track

th :: Track -> P
th = head . list

notFull :: Track -> Bool
notFull t = length (list t) < 9

-- Знаходить переможця у треку: 
-- 1 - переміг Х, -1 - переміг О, 0 - нема переможця
-- Track [8,3,4,1,0] -> 1
winner :: Track -> C
winner (Track ns)
  | testX = 1
  | testO = -1
  | otherwise = 0
  where
      wins
        = [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7],
           [2, 5, 8], [0, 4, 8], [2, 4, 6]]
      n = length ns - 1
      sn = reverse ns
      xx = [sn !! i | i <- [0, 2 .. n]]
      oo = [sn !! i | i <- [1, 3 .. n]]
      testX = any (`subset` xx) wins
      testO = any (`subset` oo) wins
      subset ss s = all (`elem` s) ss

---------------------------------------------------------------

_max, _min :: M
(_min, _max) = (1, 2)   -- enum

-- Обирає кращий за оцінкою хід, зроблений із неповного треку 
step :: Track
  -> M   -- 1 = minimum, 2 = maximum 
  -> P
step track m = snd $ maxmin costs
 where
   maxmin = if m == _min then minimum else maximum
   costs = map (\b -> (estimate (b ~ track) m, b)) blanks  -- [(оцінка, хід)]
   blanks = [0..8] \\ list track

-- Оцінює заданий трек 
estimate :: Track
   -> M    -- 1 = minimum, 2 = maximum 
   -> C    -- 1, -1,  0
estimate track m | (length . list) track < 9 =
   let c = winner track in
      if c /= 0
         then c
         else estimate (p ~ track) m'
         where
            m' = _min + _max - m
            p = step track m'
estimate track _ = winner track

-- ---------------------------------------------------------------------
run = do
   let t = Track []
   play t
   putStr "\27[5;1fContinue? y(def) | n > "
   hFlush stdout
   x <- getLine
   when (x == "y" || x == "Y" || x == "" ) run

play track = do
   drawBoard track "-------"
   putStr "\27[5;1f>>>"    -- line 5; pos 1
   hFlush stdout
   line <- getLine
   when (line /= "") $ play0 track line

play0 track line = do
   let n = readMaybe line
   case n of
      Nothing -> play track
      Just n' -> 
         if n' `elem` [0..8] \\ list track
            then play1 track n'
            else play track

play1 track n = do
  let trackX = n ~ track
  drawBoard trackX "Let me think ..."
  if winner trackX /= 1
     then play2 trackX
     else drawBoard trackX "Cross won!"

play2 trackX = do
  if notFull trackX
     then play3 trackX
     else drawBoard trackX "It's draw."

play3 trackX = do
   let posO = step trackX _min
   let trackO = posO ~ trackX
   if winner trackO /= (-1)
      then play trackO
      else drawBoard trackO "Zero won!"
