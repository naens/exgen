import Text.Printf
import System.Random
import Control.Monad
import Data.Char

-- determinant of a 2x2 matrix of integers

data Matrix = Matrix {rows :: Int, cols :: Int, values :: [Double]}
instance Show Matrix where
  show m = show_rows 0
    where
      show_rows :: Int -> String
      show_rows i
        | i < (rows m) = (show_line 0 (i*(cols m))) ++ "\n" ++ (show_rows (i+1))
        | otherwise = ""
        where
          show_line :: Int -> Int -> String
          show_line j start
            | j < (cols m) - 1 = (showf ((values m)!!(j+start))) ++ " " ++ (show_line (j+1) start)
            | j == (cols m) - 1 = showf ((values m)!!(j+start))
            | otherwise = ""
            where showf x = printf "%.f" x

random_number :: IO Double
random_number = do
  n <- randomRIO (0::Int, 8)
  return (fromIntegral n)

gen_matrix :: Int -> Int -> IO Matrix
gen_matrix rows cols = do
  g <- newStdGen
  values <- replicateM (rows * cols) random_number
  return Matrix {rows=rows, cols=cols, values=values}

determinant :: Matrix -> Double
determinant m =
  let v = (values m)
  in (v!!0)*(v!!3)-(v!!1)*(v!!2)

det_2x2 :: IO ()
det_2x2 = do
  let result = -1
  m <- gen_matrix 2 2
  putStrLn (show m)
  let d = determinant m
  putStrLn (printf "%.2f" d)



main = do
  det_2x2

