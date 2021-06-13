-- {-# LANGUAGE DeriveDataTypeable #-}

module Main where

-- import Lib

import Control.Monad.State
import Data.List (transpose)
import Data.Vector (snoc)
import Numeric.LinearAlgebra
  ( Linear (scale),
    Matrix,
    Vector,
    cmap,
    matrix,
    maxIndex,
    outer,
    vector,
    (#>),
  )
import Text.CSV (parseCSVFromFile)

type WeightValue = Matrix Double

type WeightState = Matrix Double

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
  Vector Double -> -- x - Single input vectoror
  Vector Double -> -- l - label
  State WeightState () -- Weights
stocTrain x l = do
  w <- get
  let error = predict w x - l
      sigDeriv = error * cmap (1 -) error
      delta = scale 0.01 $ sigDeriv `outer` x
      newWeights = w - delta
  put newWeights

train ::
  Int -> -- t - number of epochs to train on
  [Vector Double] -> -- x - An epoch of inputs
  [Vector Double] -> -- l - An epoch of labels
  State WeightState WeightValue -- Monad stuff
train t xs ls
  | t == 0 = get
  | otherwise = do
    trainEpoch xs ls
    train (t - 1) xs ls

trainEpoch ::
  [Vector Double] -> -- x - An epoch of inputs
  [Vector Double] -> -- l - An epoch of labels
  State WeightState WeightValue -- Weights
trainEpoch [] [] = get
trainEpoch (x : xs) (l : ls) = do
  stocTrain x l
  trainEpoch xs ls

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
    w = evalState (train 1 xTrain lTrain) initW
    (xTrain, lTrain) = csvToInputsLabels trainCSV
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
  -- testCSV <- parseCSVFromFile "./data/mnist_test.csv"
  -- trainCSV <- parseCSVFromFile "./data/mnist_train.csv"
  -- case testCSV of
  --   Left err -> print err
  --   Right testCSV ->
  --     case trainCSV of
  --       Left err -> print err
  --       Right trainCSV -> print (run testCSV testCSV)

  let inputs = map vector [[0, 0, 1], [0, 1, 1], [1, 0, 1], [1, 1, 1]]
  let labels = map vector [[0, 1], [0, 1], [0, 1], [1, 0]]
  let weights = matrix 3 (replicate 6 0)
  print "Initialized Weights"
  print weights
  print "target"
  print labels
  print "untrained prediction"
  print $ predictEpoch weights inputs
  print $ test weights inputs labels
  let trainedWeights = evalState (train 10 inputs labels) weights
  print "trained weights"
  print trainedWeights
  print "target"
  print labels
  print "trained prediciton"
  print $ predictEpoch trainedWeights inputs
  print "Test"
  print $ test trainedWeights inputs labels