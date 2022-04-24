module Main where

import TicTacToe ( run )
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let deepStr = if null args then "0" else head args
    let deep = read deepStr :: Int
    run deep