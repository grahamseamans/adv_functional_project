module Lib where

import Data.List ()
import Numeric.LinearAlgebra
  ( Linear (scale),
    Matrix,
    Vector,
    cmap,
    maxIndex,
    outer,
    (#>),
  )
import Text.CSV ()

{-
Sooooooooooooo
What do I need to do
  Need to make the output a vector
  make the weights a matrix
  make the learning stage be an outer product thats then
  subracted from the weights

  I need to implement sigmoid?
-}

predict ::
  Matrix Double -> -- curried - Weights
  Vector Double -> -- curried - Input vector
  Vector Double -- Guess
predict w x = cmap sigmoid w #> x

predictEpoch ::
  Matrix Double -> -- w - Weights
  [Vector Double] -> -- x - An epoch of input vectorors
  [Vector Double] -- A guess for each input in the epoch
predictEpoch w x = map (cmap sigmoid) (fmap (w #>) x)

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp (negate x))

stocTrain ::
  Matrix Double -> -- w - Weights
  Vector Double -> -- x - Single input vectoror
  Vector Double -> -- l - label
  Matrix Double -- Trained Weights
stocTrain w x l = w - delta
  where
    error = predict w x - l
    sigDeriv = error * cmap (1 -) error
    delta = scale 0.001 $ sigDeriv `outer` x

trainEpoch ::
  Matrix Double -> -- w - Weights
  [Vector Double] -> -- x - An epoch of inputs
  [Vector Double] -> -- l - An epoch of labels
  Matrix Double -- Weights
trainEpoch w (x : xs) (l : ls) = trainEpoch (stocTrain w x l) xs ls
trainEpoch w [] [] = w

train ::
  Int -> -- t - number of epochs to train on
  Matrix Double -> -- w - Weights
  [Vector Double] -> -- x - An epoch of inputs
  [Vector Double] -> -- l - An epoch of labels
  Matrix Double -- Weights
train t w xs ls
  | t == 0 = w
  | otherwise = train (t - 1) (trainEpoch w xs ls) xs ls

successRate ::
  Matrix Double -> -- w - Weights
  [Vector Double] -> -- x - An epoch of input vectors
  [Vector Double] -> -- l - An epoch of labels
  Double -- A guess for each input in the epoch
successRate w x l = intDiv correct total
  where
    correct = sum (zipWith checkSame l (predictEpoch w x))
    total = length l

intDiv :: Int -> Int -> Double
intDiv n l = fromIntegral n / fromIntegral l

-- validate w x l = fmap maxIndex (predictEpoch w x)

checkSame :: Vector Double -> Vector Double -> Int
checkSame x y
  | maxIndex x == maxIndex y = 1
  | otherwise = 0

{-
so we need to:
stick the ending bias 1 onto each input array
turn the weights into a matrix
turn the input vectors in a list of vectors
turn the labels into a list of one hot encoded vectors
-}

-- csvToTrainingPred :: [[String]] -> [Double]
-- csvToTrainingPred s = predictEpoch trainedWeights scaledInputs
--   where
--     (labels, unscaledInputs) = labelInputSplit (map (map read) (init s))
--     scaledInputs = scaleInput unscaledInputs
--     trainedWeights = train 1 scaledInputs labels [0 ..]

-- scaleInput :: [[Double]] -> [[Double]]
-- scaleInput = map (map (/ 255))

-- labelInputSplit :: [[Double]] -> ([Double], [[Double]])
-- labelInputSplit xs = (head t, transpose (tail t))
--   where
--     t = transpose xs
