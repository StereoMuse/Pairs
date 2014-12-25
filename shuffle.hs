import System.Random
import Data.Array.IO
import Control.Monad
import Data.List
import System.IO.Unsafe
 
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
	
type Field = [(State, Bool)]

type Position = (Int, Int)

data State = Black | Blue | Red | Green | Grey | Yellow | Empty
	deriving (Show, Eq)
	
count :: Int
count = 6
	
makeListOfStates :: Int -> Field
makeListOfStates count = replicate count (Blue, False) ++ replicate count (Yellow, False) ++ replicate count (Red, False) ++ replicate count (Grey, False) ++ replicate count (Green, False) ++ replicate count (Black, False)
	
main = shuffle $ makeListOfStates coun
