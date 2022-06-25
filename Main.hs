module Main where

import System.Console.ANSI
import System.IO
import Control.Monad.State
import Data.List

-- hasDuplicates :: (Ord a) => [a] -> Bool
--hasDuplicates xs = length (nub xs) /= length xs

-- Cell data structure
data Cell = Cell {cellValue::Int,isCellEditable::Bool} deriving (Eq,Ord)
instance Show Cell where
    show (Cell 0 _) = "." 
    show (Cell x _) = show x
toChar :: Cell -> Char
toChar c = head $ show c

resetCell:: Cell -> Cell
resetCell (Cell v b) 
    | b         = (Cell 0 b)
    | otherwise = (Cell v b)


-- isValid:: [Cell] -> Bool
-- isValid= not.hasDuplicates.(filter (/= (Cell 0 True))) 

-- Sudoku data structure 
data Sudoku = Sudoku [Cell] 
instance Show Sudoku where
    show (Sudoku (cs))  
     | cs == [] = ""
     | otherwise =(toChar (head cs)) : (show (Sudoku (tail cs)))

readSudoku :: String -> Sudoku
readSudoku ss = Sudoku (aux ss) where 
    aux ss
        | ss == "" = [] 
        | otherwise = Cell val (if val == 0 then True else False) : (aux (tail ss)) where
        -- | otherwise = Cell((read.pure :: Char->Int) (head ss)) False : (aux (tail ss))
            val = (read.pure :: Char->Int )  (head ss) 

resetSudoku :: Sudoku->Sudoku
resetSudoku (Sudoku cs) = Sudoku $ map resetCell cs

-- String for interface
frame="                           \n"++
      " +-------+-------+-------+ \n"++
      " | 1 2 3 | 1 2 3 | 1 2 3 | \n"++
      " | 4 5 6 | 4 5 6 | 4 5 6 | \n"++
      " | 7 8 9 | 7 8 9 | 7 8 9 | \n"++
      " +-------+-------+-------+ \n"++
      " | 1 2 3 | 1 2 3 | 1 2 3 | \n"++
      " | 4 5 6 | 4 5 6 | 4 5 6 | \n"++
      " | 7 8 9 | 7 8 9 | 7 8 9 | \n"++
      " +-------+-------+-------+ \n"++
      " | 1 2 3 | 1 2 3 | 1 2 3 | \n"++
      " | 4 5 6 | 4 5 6 | 4 5 6 | \n"++
      " | 7 8 9 | 7 8 9 | 7 8 9 | \n"++
      " +-------+-------+-------+ \n"++
      "                           \n"++
      " [1-9] - modify            \n"++
      "     d - delete            \n"++
      "     h - move left         \n"++
      "     l - move right        \n"++
      "     k - move up           \n"++
      "     j - move down         \n"++
      "     r - full reset        \n"

-- Indices of cell values in the frame
inds =take 81 ([59,61,63,67,69,71,75,77,79,87,89,91,95,97,99,103,105,107,115,117,119,123,125,127,131,133,135] ++ (map (+112) inds))


-- Replace an element in the list at position n with newval
replaceElem n newval (s:ss)
    | n==0      = newval:ss
    | otherwise = s: replaceElem (n-1) newval ss


-- Replace multiple element in the list at position n with newval
replaceElems ::  [Int]->String->String->String
replaceElems ns vs ss
    | vs==[]      = ss
    | ns==[]      = ss
    | otherwise =  replaceElems (tail ns) (tail vs) ( replaceElem (head ns) (head vs) ss)

replaceCell:: Int->Int->Sudoku->Sudoku
replaceCell n newval (Sudoku cs)  
        | isCellEditable (cs!!n) = Sudoku (replaceElem n (Cell newval True) cs)
        | otherwise = Sudoku (cs)


-- Prints Sudoku with the active field being higlighted
renderSudoku :: Sudoku->Int->IO ()
renderSudoku s@(Sudoku cs) n= do
    setCursorPosition 0 0
    setTitle "Blue Sudoku"

    setSGR [ SetConsoleIntensity NormalIntensity
           , SetColor Foreground Vivid White
           , SetColor Background Dull Blue
           ]
    clearScreen

    putStrLn  $ replaceElems inds (show  s) frame
    saveCursor 
    setCursorPosition ((div (inds!!n) 28)) (mod (inds!!n) 28) 
    setSGR [ SetConsoleIntensity NormalIntensity
           , SetColor Foreground Vivid White
           , SetColor Background Vivid Blue
           ]

    putStr (show (cs!!n))
    restoreCursor 

-- Visual interface and interactions
interactive :: Sudoku -> StateT (Int, Sudoku) IO ()
interactive s = do
        (n,s) <- get
        liftIO (renderSudoku s n)
        k <- liftIO getChar
        put (case k of 
            'l' -> (if (n+1)>80 then n else n+1, s)
            'j' -> (if (n+9)>80 then n else n+9, s)
            'h' -> (if (n-1)<0  then n else n-1, s)
            'k' -> (if (n-9)<0  then n else n-9, s)
            '9' -> (n, replaceCell n 9 s)
            '8' -> (n, replaceCell n 8 s)
            '7' -> (n, replaceCell n 7 s)
            '6' -> (n, replaceCell n 6 s)
            '5' -> (n, replaceCell n 5 s)
            '4' -> (n, replaceCell n 4 s)
            '3' -> (n, replaceCell n 3 s)
            '2' -> (n, replaceCell n 2 s)
            '1' -> (n, replaceCell n 1 s)
            'd' -> (n, replaceCell n 0 s)
            'r' -> (n, resetSudoku s)
            _   -> (n, s))
        (n,s) <- get
        interactive s

exampleEasy ="000260701680070090190004500820100040004602900050003028009300074040050036703018000"
main = do 
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        sudoku <- return $ readSudoku exampleEasy
        execStateT (interactive sudoku) (0, sudoku)
