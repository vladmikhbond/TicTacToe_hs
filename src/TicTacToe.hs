{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant where" #-}

module TicTacToe (
   run,
   drawBoard,
   Track(Track),
   costOf,
   bestStep,
   winner


) where

import Data.List (sort, (\\), find)
import Control.Monad ( when )
import Text.Read (readMaybe )
import System.IO (hFlush, stdout)
import Data.Complex (imagPart)
import Data.Maybe ( isJust, fromJust )

---------------------------------- TYPES --------------------------------------

type P = Int -- 0..8 - position
type C = Int -- -1, 1, 0 - cost
type M = Int -- 1, 2 - min | max
newtype Track = Track {list :: [P] }  deriving Show

_max, _min :: M
(_min, _max) = (1, 2)   -- enum

---------------------------------- DRAWING ------------------------------------
clrscr = "\27[0;0H\27[J"
origin =  "\27[0;0H"
red = "\27[31m"
yellow = "\27[33m"
cian = "\27[36m"
norm = "\27[m"
gray = "\27[1m\27[30m"
rc row col= "\27["++ show row ++";"++ show col ++"f"

drawBoard :: Track -> [P] -> String -> IO ()
drawBoard track comb message = do
   putStr $ clrscr ++ gray ++ "0  1  2\n3  4  5\n6  7  8" ++ origin ++ norm
   mapM_ drawChar (zip (reverse (list track)) "xoxoxoxox")
   putStrLn $ norm ++ rc 4 1 ++ "------- " ++ message
    where
      drawChar (pos, char) = let
         (r, c) = divMod pos 3
         color
            | pos `elem` comb = red
            | char == 'x' = yellow 
            | otherwise = cian
         in 
            putStr $ rc (r + 1) (3 * c + 1) ++ color ++ [char]

---------------------------------- MODEL -----------------------------

-- Знаходить комбінацію-переможця у треку: 
-- (xxx, 1) - переміг Х, (ooo,-1) - переміг О, ([], 0) - нема переможця
winner :: Track -> ([P], C)
winner (Track ns)
  | isJust winX  = (fromJust winX, 1)
  | isJust winO  = (fromJust winO, -1)
  | otherwise    = ([], 0)
  where
      wins = [[0,1,2],[3,4,5],[6,7,8],[0,3,6],[1,4,7],[2,5,8],[0,4,8],[2,4,6]]
      n = length ns - 1
      sn = reverse ns
      xxx = [sn !! i | i <- [0, 2 .. n]]
      ooo = [sn !! i | i <- [1, 3 .. n]]
      winX = find (`subset` xxx) wins
      winO = find (`subset` ooo) wins

costOf :: Track -> C
costOf = snd . winner

-- Обирає кращий за оцінкою хід, який продовжить даний трек 
bestStep :: Track
  -> M   -- 1 = minimum, 2 = maximum 
  -> Int
  -> P
bestStep track m deep = snd $ maxmin costs
 where
   maxmin = if m == _min then minimum else maximum
   costs = map (\b -> (estimate (b ~ track) m deep, b)) blanks  -- [(оцінка, хід)]
   blanks = [0..8] \\ list track

-- Оцінює заданий трек 
estimate :: Track
   -> M    -- 1 = minimum, 2 = maximum 
   -> Int
   -> C    -- 1, -1,  0
estimate track m deep
   | (length . list) track < 9 && deep > 0 =
   let c = costOf track in
      if c /= 0
         then c
         else costOf (pos ~ track)
         where
            m' = _min + _max - m
            pos = bestStep track m' (deep - 1)
estimate track _ _ = costOf track

----------------------------- UTILS ------------------------------------

subset ss s = all (`elem` s) ss

(~) :: P -> Track -> Track
pos ~ track = Track $ pos : list track

th :: Track -> P
th = head . list

full :: Track -> Bool
full track = length (list track) == 9

add deep x = let y = deep + x in
   if 0 <= y && y <= 9 then y else deep

putStrRC r c mess = do
   putStr $ rc r c ++ mess
   hFlush stdout

futureDraw track = length (list track) == 8 && costOf (x ~ track) == 0
 where
   [x] = [0..8] \\ list track

----------------------------------- DIALOG -------------------------------
run :: Int -> IO ()
run deep = do
   let track = Track []
   play track
   putStrRC 5 1 "Continue? (default)-yes, '-'-dumber, '+'-smarter, 'n'-no >>> "
   line <- getLine
   when (line == "" ) $ run deep
   when (line == "+" ) $ run (add deep 1)
   when (line == "-" ) $ run (add deep (-1))
   where
      play :: Track -> IO ()
      play track = do
         drawBoard track [] (show deep)
         putStrRC 5 1 ">>> "
         line <- getLine
         when (line /= "") $ play1 track line

      play1 :: Track -> String -> IO ()
      play1 track line = do
         let n = readMaybe line
         case n of
            Nothing -> play track
            Just n' ->
               if n' `elem` [0..8] \\ list track
               then play2 track n'
               else play track

      play2 :: Track -> P -> IO ()
      play2 track n = do
         let trackX = n ~ track
         drawBoard trackX [] "Let me think ..."
         let (xxx, costOf) = winner track
         --
         if costOf == 1
         then drawBoard trackX xxx "Cross won!"
         else if full trackX
         then drawBoard trackX [] "It's draw."
         else play3 trackX

      play3 :: Track -> IO ()
      play3 trackX = do
         let posO = bestStep trackX _min deep 
         let trackO = posO ~ trackX
         let (ooo, costOf) = winner trackO
         --
         if futureDraw trackO
         then drawBoard trackO [] "It'll be draw."
         else if costOf  == (-1)
         then drawBoard trackO ooo "Zero won!"
         else play trackO



