module Main where
import Graphics.UI.WXCore
import Graphics.UI.WX
import Data.Array.IArray
import Data.List.Split
import Data.Maybe
import Logic
import Helper
import Data.IORef
import Control.Monad as Monad
import Control.Concurrent
import Control.Monad.Cont
import Control.Monad.Trans
import Data.IORef
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic

data GameState = GameState {
	board :: IO Field,
	buttons :: [Button ()],
	memory :: (Int, Int)
}

getBtns wnd [] i btns = btns
getBtns wnd (x:brd) i btns = getBtns wnd brd (i + 1) (button wnd [text := (show i), clientSize := (sz 20 20), bgcolor := getColor (fst x) (snd x) ]:btns)

placeBtns :: (Form f, Widget w) => f -> [w] -> IO ()
placeBtns wnd btns = set wnd [layout := minsize (sz 200	200) $column 6 $ map (\x -> margin 3 $ row 6 (map widget x)) (chunksOf 6 btns)]

setCommand btns wnd ref = do
	let z = zip [1..64] btns
	forM_ z $ \p -> set (snd p) [on command := setStep wnd (fst p) ref]

main :: IO ()
main = start game

game :: IO ()
game = do
	let wndTitle = "Pairs"
	wnd <- frame [ text := wndTitle, bgcolor := white ]
	let brd = startFieldIO
	btns <- sequence $ getBtns wnd (fieldFromIO brd) 0 []
	let st = GameState brd btns (-1, -1)
	ref <- newIORef st
	setCommand btns wnd ref
	t <- timer wnd [interval := 3000, on command := clearPairBtn wnd ref]
	placeBtns wnd btns
	return()

setStep wnd k ref = do
	st <- readIORef ref
	let btns = buttons st
	let brd = board st
	let mem = memory st
	let mem' = updateMemory mem k (isEmpty (fieldFromIO brd) (intToPair k))
	let brd' = fst (clearPair brd (checkMemory mem mem'))
	let brd'' = nextStep brd' k
	updateBtns btns (fieldFromIO brd'')
	writeIORef ref (GameState brd'' btns mem')
	
	return()
	
clearPairBtn wnd ref = do
	st <- readIORef ref
	let btns = buttons st
	let brd = board st
	let mem = memory st
	let brd' = fst (clearPair brd mem)
	let mem' = clearMemory mem
	updateBtns btns (fieldFromIO brd')
	writeIORef ref (GameState brd' btns mem')
	return()

updateBtns :: [Button ()] -> Field -> IO ()
updateBtns btns brd = do
	let z = zip brd btns
	forM_ z $ \p -> set (snd p) [ bgcolor := getColor (fst (fst p)) (snd (fst p)) ]

updateMemory (-1, -1) k True = (k, -1)
updateMemory (t, -1) k True 
	| t == k = (t, -1)
	| otherwise = (t, k)
updateMemory (t, s) k True 
	| s == k = (t, s)
	| t == k = (t, s)
	| otherwise = (k, -1)
updateMemory (t, s) k False = (t, s)

checkMemory (t, k) (s, p)
	| t == s && k == p = (-1, -1)
	| otherwise = (t, k)
	
clearMemory (-1, -1) = (-1, -1)
clearMemory (t, -1) = (t, -1)
clearMemory (-1, t) = (-1, t)
clearMemory (k, t) = (-1, -1)