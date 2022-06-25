module Main where

import System.Console.ANSI
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Data.List
hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

type CellPosition = (Int, Int)


data Cell = Cell Int deriving (Eq,Ord)
instance Show Cell where
    show (Cell 0) = "." 
    show (Cell x) = show x

toChar (Cell 0) = '.'
toChar (Cell x) = head $ show x

isValid:: [Cell] -> Bool
isValid= not.hasDuplicates.(filter (/= (Cell 0))) 

reportPos :: IO (Maybe (Int, Int))
reportPos = do
    pos <- getCursorPosition
    putStrLn $ show pos
    return $ pos

data Sudoku = Sudoku [Cell] 
instance Show Sudoku where
        show (Sudoku (cs))  
            | cs == [] = ""
            | otherwise =(toChar (head cs)) : (show (Sudoku (tail cs)))

sudoku= Sudoku (replicate 81 (Cell 0))

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
      " h - move left             \n"++
      " l - move right            \n"++
      " k - move up               \n"++
      " j - move down             \n"++
      " d - delete                \n"++
      "                           \n"


replaceElem n newval (s:ss)
    | n==0      = newval:ss
    | otherwise = s: replaceElem (n-1) newval ss


replaceAll ::  [Int]->String->String->String
replaceAll ns vs ss
    | vs==[]      = ss
    | ns==[]      = ss
    | otherwise =  replaceAll (tail ns) (tail vs) ( replaceElem (head ns) (head vs) ss)

replaceCell:: Int->Int->Sudoku->Sudoku
replaceCell n newval (Sudoku cs) = Sudoku (replaceElem n (Cell newval) cs)


inds =take 81 ([59,61,63,67,69,71,75,77,79,87,89,91,95,97,99,103,105,107,115,117,119,123,125,127,131,133,135] ++ (map (+112) inds))


-- Prints Sudoku with the active field being higlighted
renderSudoku :: Sudoku->Int->IO ()
renderSudoku s@(Sudoku cs) n= do
    setCursorPosition 0 0
    setTitle "Sudoku"

    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Blue
           , SetColor Background Vivid White
           ]
    setSGR [ SetConsoleIntensity NormalIntensity
           , SetColor Foreground Vivid White
           , SetColor Background Dull Blue
           ]
    clearScreen

    putStrLn  $ replaceAll inds (show  s) frame
    saveCursor 
    setCursorPosition ((div (inds!!n) 28)) (mod (inds!!n) 28) 
    setSGR [ SetConsoleIntensity NormalIntensity
           , SetColor Foreground Vivid White
           , SetColor Background Vivid Blue
           ]

    putStr (show (cs!!n))
    
    restoreCursor 

aux :: Sudoku -> StateT (Int, Sudoku) IO ()
aux s = do
        (n,s) <- get
        k <- liftIO getChar
        liftIO $ print (n)
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
            _   -> (n, s))
        (n,s) <- get
        liftIO (renderSudoku s n)
        aux s
moveCursor :: Sudoku -> IO (Int, Sudoku)
moveCursor s = execStateT (aux s) (0, s)

main = do 
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        renderSudoku sudoku 0
        moveCursor sudoku 
