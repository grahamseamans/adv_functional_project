{-# LANGUAGE DeriveDataTypeable #-}

module Main where

-- import Lib

import Data.List
import Data.Vector (snoc)
import Numeric.LinearAlgebra
import Text.CSV

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
    delta = scale 0.01 $ sigDeriv `outer` x

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

test ::
  Matrix Double -> -- w - Weights
  [Vector Double] -> -- x - An epoch of input vectors
  [Vector Double] -> -- l - An epoch of labels
  Double -- A guess for each input in the epoch
test w x l = intDiv correct total
  where
    correct = sum (zipWith checkSame l (predictEpoch w x))
    total = length l

intDiv :: Int -> Int -> Double
intDiv n l = fromIntegral n / fromIntegral l

checkSame :: Vector Double -> Vector Double -> Int
checkSame x y
  | maxIndex x == maxIndex y = 1
  | otherwise = 0

run ::
  [[String]] -> -- tesCSV - read csv of test data
  [[String]] -> --  trainCSV - read csv of train data
  Double -- correct / total
run testCSV trainCSV = test w xTest lTest
  where
    (xTest, lTest) = csvToInputsLabels testCSV
    (xTrain, lTrain) = csvToInputsLabels trainCSV
    w = train 1 initW xTrain lTrain
    initW = matrix 785 (replicate (785 * 10) 0)

csvToTrainingPred :: [[String]] -> [Vector Double]
csvToTrainingPred csv = predictEpoch w xs
  where
    w = train 1 initW xs ls
    (xs, ls) = csvToInputsLabels csv
    initW = matrix 785 (replicate (785 * 10) 0)

csvToInputsLabels ::
  [[String]] -> -- read csv
  ([Vector Double], [Vector Double]) -- (x,l)
csvToInputsLabels csv = (finInputs, oneHotLabels)
  where
    oneHotLabels = fmap oneHotVector rawLabels
    finInputs = fmap vector scaledBiasedInputs
    scaledBiasedInputs = fmap (++ [1.0]) (scaleInput rawInputs)
    (rawLabels, rawInputs) = labelInputSplit (map (map read) (init csv))

scaleInput :: [[Double]] -> [[Double]]
scaleInput = map (map (/ 255))

labelInputSplit :: [[Double]] -> ([Double], [[Double]])
labelInputSplit xs = (head t, transpose (tail t))
  where
    t = transpose xs

oneHotVector :: Double -> Vector Double
oneHotVector x = vector $ replicate int 0 ++ [1] ++ replicate (9 - int) 0
  where
    int = round x

main :: IO ()
main = do
  testCSV <- parseCSVFromFile "./data/mnist_test_short.csv"
  trainCSV <- parseCSVFromFile "./data/mnist_train_short.csv"
  case testCSV of
    Left err -> print err
    Right testCSV ->
      case trainCSV of
        Left err -> print err
        Right trainCSV -> print (run testCSV trainCSV)

-- let inputs = map vector [[0, 0, 1], [0, 1, 1], [1, 0, 1], [1, 1, 1]]
-- let labels = map vector [[0, 1], [0, 1], [0, 1], [1, 0]]
-- let weights = matrix 3 (replicate 6 0)
-- print "Initialized Weights"
-- print weights
-- print "target"
-- print labels
-- print "untrained prediction"
-- print $ predictEpoch weights inputs
-- let trainedWeights = train 100 weights inputs labels
-- print "trained weights"
-- print trainedWeights
-- print "target"
-- print labels
-- print "trained prediciton"
-- print $ predictEpoch trainedWeights inputs
-- print "Test"
-- print $ test trainedWeights inputs labels