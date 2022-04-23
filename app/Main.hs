module Main where

import TicTacToe ( run ) 
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs                  
    let deep = read (head args ) :: Int
    run deep