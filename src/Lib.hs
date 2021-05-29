module Lib
  ( someFunc,
    train,
    --   predict,
    --   elemSubtract,
    --   initWeights,
    --   run,
  )
where

import Control.Monad (replicateM, when)
import System.Random (getStdRandom, randomR)

type Vect = [Float]

someFunc :: IO ()
someFunc = putStrLn "yeet"

predict ::
  Vect -> -- Weights
  Vect -> -- Input Vector
  Float -- Guess
predict w x
  | dot w x > 0 = 1
  | otherwise = 0

predEpoch ::
  Vect -> -- x - Weights
  [Vect] -> -- curried - An epoc of input vectors
  [Float] -- A guess for each input in the epoch
predEpoch w = map (predict w)

stocTrain ::
  Vect -> -- w - Weights
  Vect -> -- x - Single input vector
  Float -> -- l - label
  Vect -- Trained Weights
stocTrain w x l = elemSubtract w (map (0.1 * error *) x)
  where
    error = predict w x - l

runEpoch ::
  [Vect] -> -- x - An epoch of inputs
  Vect -> -- l - An epoch of labels
  Vect -> -- w - Weights
  Vect -- Weights
runEpoch [] [] w = w
runEpoch (x : xs) (l : ls) w = runEpoch xs ls (stocTrain w x l)

train ::
  Int -> -- t - number of epochs to train on
  [Vect] -> -- x - An epoch of inputs
  Vect -> -- l - An epoch of labels
  Vect -> -- w - Weights
  Vect -- Weights
train t xs ls w
  | t == 0 = w
  | otherwise = train (t - 1) xs ls (runEpoch xs ls w)

dot :: Vect -> Vect -> Float
dot x y = sum (zipWith (*) x y)

elemSubtract :: Vect -> Vect -> Vect
elemSubtract (a : as) (b : bs) = a - b : elemSubtract as bs
elemSubtract _ _ = []

elemAdd :: Vect -> Vect -> Vect
elemAdd (a : as) (b : bs) = a + b : elemAdd as bs
elemAdd _ _ = []

-- initWeights :: Int -> IO [Float]
-- initWeights 0 = pure []
-- initWeights numWeights = do
--   let interval = randomR (-0.5, 0.5)
--   replicateM numWeights (getStdRandom interval)
