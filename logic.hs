module Logic where
--PAIR
import Data.List
import System.IO.Unsafe
import Helper



	
startField :: Field
startField = fieldFromIO (shuffle (makeListOfStates count))

startFieldIO :: IO Field
startFieldIO = shuffle $ makeListOfStates count

fieldToIO :: Field -> IO Field
fieldToIO = return

fieldFromIO :: IO Field -> Field
fieldFromIO = unsafePerformIO

intToPair k
	| (k `mod` count) == 0 = ((k `div` count), count)
	| otherwise = ((k `div` count) + 1, (k `mod` count))

nextStep :: IO Field -> Int -> IO Field
nextStep field k = fieldToIO $ changeVisible (fieldFromIO field) (intToPair k) True

clearPair :: IO Field -> (Int, Int) -> (IO Field, Bool)
clearPair field (-1, -1) = (field, False)
clearPair field (-1, k) = (field, False)
clearPair field (k, -1) = (field, False)
clearPair field (t, k) = (fieldToIO (changeVisible ( changeVisible (fieldFromIO field) (intToPair t) (checkColor (fieldFromIO field) t k) ) (intToPair k) (checkColor (fieldFromIO field) t k) ), True)

checkColor field t k 
	| (fst (getState field $ intToPair t)) == (fst (getState field $ intToPair k)) = True
	| otherwise = False