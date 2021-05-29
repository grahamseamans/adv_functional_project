module Main where

import Lib

-- run :: Int -> [Inputs] -> [Label] -> Weights -> Weights

main :: IO ()
main = do
  weights <- initWeights 2
  let finalWeights = run 10 [[0, 0], [0, 1], [1, 0], [1, 1]] [0, 0, 0, 1] [0.1, 0.1] -- weights
  print finalWeights
