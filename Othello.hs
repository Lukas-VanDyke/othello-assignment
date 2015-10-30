import Debug.Trace 
import Control.Monad.Trans.State.Lazy
import Data.Maybe (fromJust, isNothing)
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
        args <- getArgs
        if args == [] then do 
                                        putStrLn "\nPossible strategies:\n  easy\n  corners\n  greedy\nEnter the strategy for BLACK: "
                                        modeB <- getLine
                                        putStrLn "\nEnter the strategy for WHITE: "
                                        modeW <- getLine
                                        if ((modeB == "easy" || modeB == "corners" || modeB == "greedy") && (modeW == "easy" || modeW == "corners" || modeW == "greedy"))
                                                then do
                                                        print initBoard
                                                        gameLoop initBoard (stringToChooser modeB) (stringToChooser modeW) B W 0 
                                        else do
                                                putStrLn "\nInvalid strategy!\nPossible strategies:\n  easy\n  corners\n  greedy\n"
        else do 
                if not (("easy" `elem` args && "corners" `elem` args) ||  ("easy" `elem` args && "greedy" `elem` args) || ("greedy" `elem` args && "corners" `elem` args) || (args == ["easy", "easy"]) || (args == ["greedy", "greedy"]) || (args == ["corners", "corners"])) then do
                        putStrLn "\nInvalid strategy!\nPossible strategies:\n  easy\n  corners\n  greedy\n"
                else do
                        putStrLn "\nThe initial board:"
                        print initBoard
                        gameLoop initBoard modeB modeW B W 0 
                        where (modeB, modeW) = (stringToChooser (head args), stringToChooser (last args))

---GameLoop---------------------------------------------------------
{- | 

This is the game loop. It basically cycles through the players until a 
player has passed twice or the board is full at which point it will count the
cells both players have and display a winner!

-}     
gameLoop :: GameState -> Chooser -> Chooser -> Cell -> Cell -> Int -> IO()
gameLoop b x y active inactive bpass = do
        print b
        if bpass == 3 then do 
                                finalScore (theBoard b) 0 0 0 0 
        else do
                let mv = x b active
                        in case mv of 
                                Nothing -> do (gameLoop GameState {play = (playerOf active, Passed), theBoard = (theBoard b)} y x inactive active (bpass+1))
                                (Just pt) -> do (gameLoop GameState {play = (playerOf active, (Played (fst pt, snd pt))), theBoard = replace2 (doFlip (theBoard b) active (fst pt) (snd pt) 0) pt active}  y x inactive active 0)
             
-- Black wins!  Black (firstAvailable): 4  White (reallyStupid): 1                
finalScore :: Board -> Int -> Int -> Int -> Int -> IO()
finalScore b black white x 8 = finalScore b black white (x+1) 0
finalScore b black white 8 y = do 
        if black > white then putStrLn ("Black Wins! Black: " ++ (show black) ++ " White: " ++ (show white))
        else if white > black then putStrLn ("White Wins! Black: " ++ (show black) ++ " White: " ++ (show white))
        else putStrLn ("Draw! Black: " ++ (show black) ++ " White: " ++ (show white))
finalScore b black white x y =
        if ((b !! y) !! x) == W then finalScore b black (white+1) x (y+1)
        else if ((b !! y) !! x) == B then finalScore b (black+1) white x (y+1)
        else finalScore b black white x (y+1)

        
---Strategies-------------------------------------------------------

{- | 

This is where strategies are.

There are 3 of them : easy, corners, and greedy.

There is also a fourth called "dummy" but it is not used in the game and is only
there to help take in IO from command line. 

Strategies are of type Chooser that will take in game state and cell and return a 
position on the board (int,int) or nothing if the player cannot make any moves.

easy: will find first available move and play it
corners: will check the corners, play them if available else do what easy would do
greedy: will check all available moves and make the move that will flip the highest number of discs

-}     

stringToChooser :: String -> Chooser
stringToChooser x
        | x == "easy" = easy
        | x == "corners" = corners
        | x == "greedy" = greedy
        | otherwise = dummy
      
type Chooser = GameState -> Cell -> Maybe (Int,Int)

dummy:: Chooser
dummy b c = Just (-1, -1)

easy :: Chooser
easy b c =
        if fst point == (-1) then Nothing
        else Just point
        where point = getPointEasy (theBoard b) c 0 0

corners :: Chooser
corners b c = 
        if (fst $ findMove (theBoard b) c 0 0) && (((d !! 0) !! 0) == E) then Just (0, 0)
        else if (fst $ findMove (theBoard b) c 0 7) && (((d !! 7) !! 0) == E) then Just (0, 7)
        else if (fst $ findMove (theBoard b) c 7 0) && (((d !! 0) !! 7) == E) then Just (7, 0)
        else if (fst $ findMove (theBoard b) c 7 7) && (((d !! 7) !! 7) == E) then Just (7, 7)
        else let mv = easy b c
                      in case mv of
                                Nothing -> Nothing
                                (Just pt) -> Just pt
        where d = theBoard b
        
greedy :: Chooser
greedy b c =
        if fst point == (-1) then Nothing
        else Just point
        where point = getPointGreedy (theBoard b) c [] [] 0 0

---DirectionalFunctions---------------------------------------------

{- | 

The core of this program. the getPoint____ functions select points for their respective strategies.
Max point works together with getPointGreedy to find the maximum amount of flips per turn by making a list
with all possible moves and their weight.

FindMove will return a (True, flips on said move) if it finds a move and it runs through every single non
empty slot.
 
Do flip is the main flip function which directs other fuctions to perform flips based on the return value
of check(direction) 

The flip functions are called on the cell which is supposed to be flipped and will follow their named direction
until they hit a cell of the same type of the call.

Check direction functins check the cell next to the original cell in the named direction. They return True when
the cell will cause flips if it is picked.

Check direction fuctions also return an int that is the amount of discs that will be flipped by said move. This
value is only used by the greedy AI

-} 



getPointEasy :: Board -> Cell -> Int -> Int -> (Int, Int)
getPointEasy b c x 8 = getPointEasy b c (x+1) 0
getPointEasy b c 8 y = (-1, -1)
getPointEasy b c x y =
        if ((b !! y) !! x) /= E then getPointEasy b c x (y+1)
        else if fst $ findMove b c x y then (x, y)
        else getPointEasy b c x (y+1)

        
getPointGreedy :: Board -> Cell -> [((Int, Int), Int)] -> [Int] -> Int -> Int -> (Int, Int)
getPointGreedy b c l n x 8 = getPointGreedy b c l n (x+1) 0
getPointGreedy b c l [] 8 y = (-1, -1)
getPointGreedy b c l n 8 y = maxPoint l (maximum n)
getPointGreedy b c l n x y =
        if ((b !! y) !! x) /= E then getPointGreedy b c l n x (y+1)
        else if fst $ findMove b c x y then getPointGreedy b c (((x, y) , (snd $ findMove b c x y)) : l) ((snd $ findMove b c x y) : n) x (y+1)
        else getPointGreedy b c l n x (y+1)
        
maxPoint :: [((Int, Int), Int)] -> Int -> (Int, Int)
maxPoint  [x]  n = fst x
maxPoint (x:xs) n
        | snd x == n = fst x
        | otherwise = maxPoint xs n



findMove :: Board -> Cell -> Int -> Int -> (Bool, Int)
findMove b c x y
        | fst (checkRight b c x (y+1) 0) = (True, snd (checkRight b c x (y+1) 0))
        | fst (checkDownRight b c (x+1) (y+1) 0)  = (True, snd (checkDownRight b c (x+1) (y+1) 0))
        | fst (checkDown b c (x+1) y 0)  = (True, snd (checkDown b c (x+1) y 0))
        | fst (checkDownLeft b c (x+1) (y-1) 0) = (True, snd (checkDownLeft b c (x+1) (y-1) 0))
        | fst (checkLeft b c x (y-1) 0) = (True, snd (checkLeft b c x (y-1) 0))
        | fst (checkUpLeft b c (x-1) (y-1) 0) = (True, snd (checkUpLeft b c (x-1) (y-1) 0))
        | fst (checkUp b c (x-1) y 0) = (True, snd (checkUp b c (x-1) y 0))
        | fst (checkUpRight b c (x-1) (y+1) 0) = (True, snd (checkUpRight b c (x-1) (y+1) 0))
        | otherwise = (False, -1)

doFlip :: Board -> Cell -> Int -> Int -> Int -> Board
doFlip b c x y 0 =
        if fst $ checkLeft b c x (y-1) 0 then doFlip (flipLeft b c x y) c x y 1
        else doFlip b c x y 1
doFlip b c x y 1 =
        if fst $ checkUpLeft b c (x-1) (y-1) 0 then doFlip (flipUpLeft b c x y) c x y 2
        else doFlip b c x y 2
doFlip b c x y 2 =
        if fst $ checkUp b c (x-1) y 0 then doFlip (flipUp b c x y) c x y 3
        else doFlip b c x y 3
doFlip b c x y 3 =
        if fst $ checkUpRight b c (x-1) (y+1) 0 then doFlip (flipUpRight b c x y) c x y 4
        else doFlip b c x y 4
doFlip b c x y 4 =
        if fst $ checkRight b c x (y+1) 0 then doFlip (flipRight b c x y) c x y 5
        else doFlip b c x y 5
doFlip b c x y 5 =
        if fst $ checkDownRight b c (x+1) (y+1) 0 then doFlip (flipDownRight b c x y) c x y 6
        else doFlip b c x y 6
doFlip b c x y 6 =
        if fst $ checkDown b c (x+1) y 0 then doFlip (flipDown b c x y) c x y 7
        else doFlip b c x y 7
doFlip b c x y 7 =
        if fst $ checkDownLeft b c (x+1) (y-1) 0 then doFlip (flipDownLeft b c x y) c x y 8
        else doFlip b c x y 8
doFlip b c x y n = b

checkLeft :: Board -> Cell -> Int -> Int -> Int -> (Bool, Int)        
checkLeft b c x (-1) z = (False, z)
checkLeft b c x y z =
        if ((b !! y) !! x) == otherCell c then checkLeft b c x (y-1) (z+1)
        else if ((b !! y) !! x) == c && (z > 0) then (True, z)
        else (False, z)
 
checkUpLeft :: Board -> Cell -> Int -> Int -> Int -> (Bool, Int)     
checkUpLeft b c x (-1) z = (False, z)
checkUpLeft b c (-1) y z = (False, z)
checkUpLeft b c x y z =
        if ((b !! y) !! x) == otherCell c then checkUpLeft b c (x-1) (y-1) (z+1)
        else if ((b !! y) !! x) == c && (z > 0) then (True, z)
        else (False, z)

checkUp :: Board -> Cell -> Int -> Int -> Int -> (Bool, Int)            
checkUp b c (-1) y z = (False, z)
checkUp b c x y z =
        if ((b !! y) !! x) == otherCell c then checkUp b c (x-1) y (z+1)
        else if ((b !! y) !! x) == c && (z > 0) then (True, z)
        else (False, z)

checkUpRight :: Board -> Cell -> Int -> Int -> Int -> (Bool, Int)            
checkUpRight b c x 8 z = (False, z)
checkUpRight b c (-1) y z = (False, z)
checkUpRight b c x y z = 
        if ((b !! y) !! x) == otherCell c then checkUpRight b c (x-1) (y+1) (z+1)
        else if ((b !! y) !! x) == c && (z > 0) then (True, z)
        else (False, z)
         
checkRight :: Board -> Cell -> Int -> Int -> Int -> (Bool, Int)     
checkRight b c x 8 z = (False, z)
checkRight b c x y z =
        if ((b !! y) !! x) == otherCell c then checkRight b c x (y+1) (z+1)
        else if ((b !! y) !! x) == c && (z > 0) then (True, z)
        else (False, z)

checkDownRight :: Board -> Cell -> Int -> Int -> Int -> (Bool, Int)            
checkDownRight b c x 8 z = (False, z)
checkDownRight b c 8 y z = (False, z)
checkDownRight b c x y z =
        if ((b !! y) !! x) == otherCell c then checkDownRight b c (x+1) (y+1) (z+1)
        else if ((b !! y) !! x) == c && (z > 0) then (True, z)
        else (False, z)

checkDown :: Board -> Cell -> Int -> Int -> Int -> (Bool, Int)            
checkDown b c 8 y z = (False, z)
checkDown b c x y z =
        if ((b !! y) !! x) == otherCell c then checkDown b c (x+1) y (z+1)
        else if ((b !! y) !! x) == c && (z > 0) then (True, z)
        else (False, z)

checkDownLeft :: Board -> Cell -> Int -> Int -> Int -> (Bool, Int)            
checkDownLeft b c x (-1) z = (False, z)
checkDownLeft b c 8 y z = (False, z)
checkDownLeft b c x y z =
        if ((b !! y) !! x) == otherCell c then checkDownLeft b c (x+1) (y-1) (z+1)
        else if ((b !! y) !! x) == c && (z > 0) then (True, z)
        else (False, z)

flipLeft :: Board -> Cell -> Int -> Int -> Board
flipLeft b c x y = 
        if ((b !! (y-1)) !! x) == otherCell c then flipLeft (replace2 b (x, (y-1)) c) c x (y-1)
        else b
        
flipUpLeft :: Board -> Cell -> Int -> Int -> Board
flipUpLeft b c x y =
        if ((b !! (y-1)) !! (x-1)) == otherCell c then flipUpLeft (replace2 b ((x-1), (y-1)) c) c (x-1) (y-1)
        else b
        
flipUp :: Board -> Cell -> Int -> Int -> Board
flipUp b c x y = 
        if ((b !! y) !! (x-1)) == otherCell c then flipUp (replace2 b ((x-1), y) c) c (x-1) y
        else b
        
flipUpRight :: Board -> Cell -> Int -> Int -> Board
flipUpRight b c x y =
        if ((b !! (y+1)) !! (x-1)) == otherCell c then flipUpRight (replace2 b ((x-1), (y+1)) c) c (x-1) (y+1)
        else b

flipRight :: Board -> Cell -> Int -> Int -> Board
flipRight b c x y =
        if ((b !! (y+1)) !! x) == otherCell c then flipRight (replace2 b (x, (y+1)) c) c x (y+1)
        else b

flipDownRight :: Board -> Cell -> Int -> Int -> Board
flipDownRight b c x y =
        if ((b !! (y+1)) !! (x+1)) == otherCell c then flipDownRight (replace2 b ((x+1), (y+1)) c) c (x+1) (y+1)
        else b

flipDown :: Board -> Cell -> Int -> Int -> Board
flipDown b c x y =
        if ((b !! y) !! (x+1)) == otherCell c then flipDown (replace2 b ((x+1), y) c) c (x+1) y
        else b

flipDownLeft :: Board -> Cell -> Int -> Int -> Board
flipDownLeft b c x y =
        if ((b !! (y-1)) !! (x+1)) == otherCell c then flipDownLeft (replace2 b ((x+1), (y-1)) c) c (x+1) (y-1)
        else b


---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                    in  (if null zs then (if null ys then [] else init ys) else ys) ++ [elem] ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

