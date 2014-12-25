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

getColor st False = white
getColor Black True = black
getColor Empty True = white
getColor Red True = red
getColor Blue True = blue

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