module Helper where
--PAIR
import Data.List
import System.IO.Unsafe
import Graphics.UI.WXCore
import Graphics.UI.WX

type Field = [(State, Bool)]

type Position = (Int, Int)

data State = Black | Blue | Red | Green | Grey | Yellow | Empty
	deriving (Show, Eq)
	
count :: Int
count = 6

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
	
makeListOfStates :: Int -> Field
makeListOfStates count = replicate count (Blue, False) ++ replicate count (Yellow, False) ++ replicate count (Red, False) ++ replicate count (Grey, False) ++ replicate count (Green, False) ++ replicate count (Black, False)

getColor st False = white
getColor Black True = black
getColor Empty True = white
getColor Red True = red
getColor Blue True = blue
getColor Green True = green
getColor Grey True = grey
getColor Yellow True = yellow

getState :: Field -> Position -> (State, Bool)
getState field pos = field !! ((fst pos - 1) * count + (snd pos - 1))

setState :: Field -> (State, Bool) -> Position -> Field
setState field state pos = take ((fst pos - 1) * count + (snd pos - 1)) field ++ [state] ++ drop ((fst pos - 1) * count + snd pos) field

changeVisible :: Field -> Position -> Bool -> Field
changeVisible field pos vis = setState field ((fst (getState field pos)), vis) pos

toggleVisible :: Field -> Position -> Field
toggleVisible field pos = setState field ((fst (getState field pos)), (toggleBool $ snd (getState field pos))) pos

isEmpty field pos = toggleBool (snd $ getState field pos)

toggleBool False = True
toggleBool True = False