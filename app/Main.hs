module Main where

-- import Lib (train)
import Data.List
-- import qualified Data.vector as V
import Numeric.LinearAlgebra
import Text.CSV

predict ::
  Vector Double -> -- curried - Weights
  Vector Double -> -- curried - Input vectoror
  Double -- Guess
predict w x = w <.> x

predictEpoch ::
  Vector Double -> -- w - Weights
  [Vector Double] -> -- curried - An epoch of input vectorors
  [Double] -- A guess for each input in the epoch
predictEpoch w = map (`dot` w)

stocTrain ::
  Vector Double -> -- w - Weights
  Vector Double -> -- x - Single input vectoror
  Double -> -- l - label
  Vector Double -- Trained Weights
stocTrain w x l = w - scale (0.1 * error) x
  where
    error = predict w x - l

trainEpoch ::
  [Vector Double] -> -- x - An epoch of inputs
  [Double] -> -- l - An epoch of labels
  Vector Double -> -- w - Weights
  Vector Double -- Weights
trainEpoch (x : xs) (l : ls) w = trainEpoch xs ls (stocTrain w x l)
trainEpoch [] [] w = w

train ::
  Int -> -- t - number of epochs to train on
  [Vector Double] -> -- x - An epoch of inputs
  [Double] -> -- l - An epoch of labels
  Vector Double -> -- w - Weights
  Vector Double -- Weights
train t xs ls w
  | t == 0 = w
  | otherwise = train (t - 1) xs ls (trainEpoch xs ls w)

-- dot :: vector -> vector -> Double
-- dot x y = sum (zipWith (*) x y)

-- elemSubtract :: vector -> vector -> vector
-- elemSubtract = zipWith (-)

-- elemAdd :: vector -> vector -> vector
-- elemAdd = zipWith (+)

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

main :: IO ()
main = do
  -- test_csv <- parseCSVFromFile "./data/mnist_test.csv"
  -- case test_csv of
  --   Left err -> print err
  --   Right csv -> print (csvToTrainingPred csv)

  -- contents <- BL.readFile "./data/train-labels-idx1-ubyte"
  -- let header = BL.take 4 contents
  -- BL.writeFile "test_write" header

  let inputs = map vector [[0, 0, 1], [0, 1, 1], [1, 0, 1], [1, 1, 1]]
  let labels = [0, 0, 0, 1]
  let weights = vector (replicate 3 0)
  print (predictEpoch weights inputs)
  let trainedWeights = train 100 inputs labels weights
  print (predictEpoch trainedWeights inputs)
  print trainedWeights
