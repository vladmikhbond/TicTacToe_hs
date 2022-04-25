
import TicTacToe (drawBoard, Track(Track), bestStep)

main :: IO ()
main = do
   print $ bestStep (toTrack $ 
      "x x -" ++
      "x o -" ++
      "o - -" ) 1 1   -- 2
   print $ bestStep (toTrack $ 
      "x - -" ++
      "o x -" ++
      "o - -" ) 2 1   -- 8 
  

-- 0x 1x 2-
-- 3x 4o 5- 
-- 6o 7- 8-
toTrack cs = let 
   cs' = filter (/=' ') cs
   csX = [i | (c, i) <- zip cs' [0..8], c == 'x']
   csO = [i | (c, i) <- zip cs' [0..8], c == 'o']
   t = concat $ zipWith (\x o -> [x, o]) csX csO
   t' = if length csX > length csO 
      then t ++ [last csX] else t
 in
   Track t'


