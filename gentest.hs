import Text.Printf
import System.Random
import Control.Monad
import Data.Char

-- determinant of a 2x2 matrix of integers

det_2x2 :: IO Integer
det_2x2 = do
  let result = -1
  m <- replicateM 4 (randomRIO (0::Int, 8))
  putStrLn (show m)
  return result


main = do
  det_2x2

