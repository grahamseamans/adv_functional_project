module Lib
  ( someFunc,
  --   predict,
  --   elemSubtract,
  --   initWeights,
  --   run,
  )
where

import Control.Monad (replicateM)
import System.Random (getStdRandom, randomR)

type Vect = [Float]

someFunc :: IO ()
someFunc = putStrLn "yeet"

predict :: Vect -> Vect -> Int
predict x w
  | dot x w > 0 = 1
  | otherwise = 0

-- learn :: Inputs -> Weights -> Threshold -> Float -> Guess -> Weights
-- learn inputs weights threshold label guess =
--   elemSubtract weights delta
--   where
--     delta = map (lRate *) slope
--     slope = map (error *) inputs
--     error = label - guess
--     lRate = 0.1

-- getDelta :: Inputs -> Weights -> Float -> Guess -> Delta
-- getDelta inputs weights label guess =
--   delta
--   where
--     delta = map (lRate *) slope
--     slope = map (error *) inputs
--     error = label - guess
--     lRate = 0.1

-- processEpoch :: [Inputs] -> [Float] -> Weights -> Weights
-- processEpoch inputs labels weights = elemSubtract weights delta
--   where
--     delta = elemDivide summedDeltas [fromIntegral len ..]
--     summedDeltas = foldl elemAdd [] (zipWith4 getDelta inputs repeatedWeights labels guesses)
--     guesses = zipWith predict repeatedWeights inputs
--     repeatedWeights = replicate len weights
--     len = length labels

-- run :: Int -> [Inputs] -> [Float] -> Weights -> Weights
-- run times inputs labels weights
--   | times == 0 = weights
--   | otherwise = run (times - 1) inputs labels (processEpoch inputs labels weights)

-- initWeights :: Int -> IO [Float]
-- initWeights 0 = pure []
-- initWeights numWeights = do
--   let interval = randomR (-0.5, 0.5)
--   replicateM numWeights (getStdRandom interval)

-- zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
-- zipWith4 f (a : as) (b : bs) (c : cs) (d : ds) = f a b c d : zipWith4 f as bs cs ds
-- zipWith4 _ _ _ _ _ = []

dot :: Vect -> Vect -> Float
dot x y = sum (zipWith (*) x y)

elemSubtract :: Vect -> Vect -> Vect
elemSubtract (a : as) (b : bs) = a - b : elemSubtract as bs
elemSubtract _ _ = []

elemAdd :: Vect -> Vect -> Vect
elemAdd (a : as) (b : bs) = a + b : elemAdd as bs
elemAdd _ _ = []