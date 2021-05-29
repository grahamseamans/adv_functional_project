module Main where

import Lib

main :: IO ()
main = do
  someFunc

-- train t xs ls w
-- let w = train 10 [[0.0, 0.0], [0.0, 1.0], [1.0, 0.0], [1.0, 1.0]] [0.0, 0.0, 0.0, 1.0] [0.1, 0.1]
-- print w
