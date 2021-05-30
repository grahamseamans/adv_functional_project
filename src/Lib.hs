module Lib where

-- import Control.Monad (replicateM, when)
-- import System.Random (getStdRandom, randomR)

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

predictEpoch ::
  Vect -> -- x - Weights
  [Vect] -> -- curried - An epoc of input vectors
  [Float] -- A guess for each input in the epoch
predictEpoch w = map (predict w)

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
elemSubtract = zipWith (-)

elemAdd :: Vect -> Vect -> Vect
elemAdd = zipWith (+)