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
        | i < (rows m) - 1 = (show_line 0 (i*(cols m))) ++ "\n" ++ (show_rows (i+1))
        | otherwise =(show_line 0 (i*(cols m)))
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

--    a11 * a22 * a33
-- +  a12 * a23 * a31
-- +  a13 * a21 * a32
-- -  a13 * a22 * a31
-- -  a11 * a23 * a32
-- -  a12 * a21 * a33
-- index(i,j) = (i-1)*3 + (j-1)

determinant :: Matrix -> Double
determinant m =
  let v = (values m)
      r = (rows m)
      c = (cols m)
  in
    if r == 1 && c == 1 then v!!0
    else if r == 2 && c == 2 then (v!!0)*(v!!3)-(v!!1)*(v!!2)
    else if r == 3 && c == 3 then
           (v!!0)*(v!!4)*(v!!8) + (v!!1)*(v!!5)*(v!!6) + (v!!2)*(v!!3)*(v!!7)
           - (v!!2)*(v!!4)*(v!!6) - (v!!0)*(v!!5)*(v!!7) - (v!!1)*(v!!3)*(v!!8)
    else error "bad matrix dimension for determinant"

-- TODO: make interactive: read answer and then compare
-- TODO: more questions: eigenvectors, eigenvalues, factorize quadratic polynomial
-- TODO: make test by combining several questions and then showing the result

det_2x2 :: IO ()
det_2x2 = do
  let result = -1
  m <- gen_matrix 2 2
  putStrLn (show m)
  let d = determinant m
  putStrLn (printf "determinant %.2f" d)


main = do
  det_2x2


