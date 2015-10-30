import Debug.Trace 
import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
import System.Random
import Data.List ((\\))
import System.Environment
import System.IO.Unsafe
import Data.Either
import OthelloTools

{- | This program is used as a template for the CPSC 449  Othello assignment.

Feel free to modify this file as you see fit.

Copyright: Copyright 2015, Rob Kremer (rkremer@ucalgary.ca), University of Calgary. 
Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose is hereby granted without fee, provided
that the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation. The University of Calgary makes no representations about the
suitability of this software for any purpose. It is provided "as is" without
express or implied warranty.

-}

---Main-------------------------------------------------------------

main = main' (unsafePerformIO getArgs)
    
{- | We have a main' IO function so that we can either:
     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
    putStrLn "\nThe initial board:"
    print initBoard
    
    putStrLn "The initial board rotated 90 degrees:"
    putBoard $ rotateClock $ theBoard initBoard
    
    putStrLn "\nThe initial board with reallyStupidStrategy having played one move (clearly illegal!):"
    let mv = reallyStupidStrategy (initBoard) B
       in case mv of
          Nothing   -> putStrLn "Black passed."
          (Just pt) -> putBoard $ replace2 (theBoard initBoard) pt B

---Strategies-------------------------------------------------------

{- | This is the type for all player functions.  A player strategy function takes a 
     board and returns a point (Int,Int) that it chooses -- this should be a legal move.  
     If the player passes, the funciton should return Nothing.
-}           
type Chooser = GameState -> Cell -> Maybe (Int,Int)

-- | This strategy lives up to it's name: it always chooses to play at cell (0,0).
reallyStupidStrategy  :: Chooser
reallyStupidStrategy b c = Just (0,0)


---Board rotations-------------------------------------------------------------

-- | Rotate a board 90 degrees clockwise.
rotateClock     :: [[a]] -> [[a]]
rotateClock       [ [a0, a1, a2, a3, a4, a5, a6, a7],
                    [b0, b1, b2, b3, b4, b5, b6, b7],
                    [c0, c1, c2, c3, c4, c5, c6, c7],
                    [d0, d1, d2, d3, d4, d5, d6, d7],
                    [e0, e1, e2, e3, e4, e5, e6, e7],
                    [f0, f1, f2, f3, f4, f5, f6, f7],
                    [g0, g1, g2, g3, g4, g5, g6, g7],
                    [h0, h1, h2, h3, h4, h5, h6, h7] ] =
                  [ [h0, g0, f0, e0, d0, c0, b0, a0],
                    [h1, g1, f1, e1, d1, c1, b1, a1],
                    [h2, g2, f2, e2, d2, c2, b2, a2],
                    [h3, g3, f3, e3, d3, c3, b3, a3],
                    [h4, g4, f4, e4, d4, c4, b4, a4],
                    [h5, g5, f5, e5, d5, c5, b5, a5],
                    [h6, g6, f6, e6, d6, c6, b6, a6],
                    [h7, g7, f7, e7, d7, c7, b7, a7] ]

-- | Rotate a board 90 degrees counter clockwise.
rotateCounter     [ [h0, g0, f0, e0, d0, c0, b0, a0],
                    [h1, g1, f1, e1, d1, c1, b1, a1],
                    [h2, g2, f2, e2, d2, c2, b2, a2],
                    [h3, g3, f3, e3, d3, c3, b3, a3],
                    [h4, g4, f4, e4, d4, c4, b4, a4],
                    [h5, g5, f5, e5, d5, c5, b5, a5],
                    [h6, g6, f6, e6, d6, c6, b6, a6],
                    [h7, g7, f7, e7, d7, c7, b7, a7] ] =
                  [ [a0, a1, a2, a3, a4, a5, a6, a7],
                    [b0, b1, b2, b3, b4, b5, b6, b7],
                    [c0, c1, c2, c3, c4, c5, c6, c7],
                    [d0, d1, d2, d3, d4, d5, d6, d7],
                    [e0, e1, e2, e3, e4, e5, e6, e7],
                    [f0, f1, f2, f3, f4, f5, f6, f7],
                    [g0, g1, g2, g3, g4, g5, g6, g7],
                    [h0, h1, h2, h3, h4, h5, h6, h7] ]

-- | These functions return a rotated point the same as the board rotations above.
type PointRotation = (Int,Int) -> (Int,Int)

-- | rotate 90 degrees clockwise.
rotatePt             :: PointRotation
rotatePt             (x,y) = (7-y, x)   
               
---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                    in  (if null zs then (if null ys then [] else init ys) else ys) ++ [elem] ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)


