module Main where

-- import Lib (train)
import Data.List
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

validate ::
  Matrix Double -> -- w - Weights
  [Vector Double] -> -- x - An epoch of input vectorors
  [Vector Double] -> -- l - An epoch of labels
  [Int] -- A guess for each input in the epoch
validate w x l = zipWith checkSame l (predictEpoch w x)

checkSame :: Vector Double -> Vector Double -> Int
checkSame x y
  | maxIndex x == maxIndex y = 1
  | otherwise = 0

stocTrain ::
  Matrix Double -> -- w - Weights
  Vector Double -> -- x - Single input vectoror
  Vector Double -> -- l - label
  Matrix Double -- Trained Weights
stocTrain w x l = w - delta
  where
    error = predict w x - l
    sigDeriv = error * cmap (1 -) error
    delta = scale 0.1 $ sigDeriv `outer` x

trainEpoch ::
  [Vector Double] -> -- x - An epoch of inputs
  [Vector Double] -> -- l - An epoch of labels
  Matrix Double -> -- w - Weights
  Matrix Double -- Weights
trainEpoch (x : xs) (l : ls) w = trainEpoch xs ls (stocTrain w x l)
trainEpoch [] [] w = w

train ::
  Int -> -- t - number of epochs to train on
  [Vector Double] -> -- x - An epoch of inputs
  [Vector Double] -> -- l - An epoch of labels
  Matrix Double -> -- w - Weights
  Matrix Double -- Weights
train t xs ls w
  | t == 0 = w
  | otherwise = train (t - 1) xs ls (trainEpoch xs ls w)

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
  let labels = map vector [[0, 1], [0, 1], [0, 1], [1, 0]]
  let weights = matrix 3 (replicate 6 0)
  print "Initialized Weights"
  print weights
  print "target"
  print labels
  print "untrained prediction"
  print (predictEpoch weights inputs)
  let trainedWeights = train 100 inputs labels weights
  print "trained weights"
  print trainedWeights
  print "target"
  print labels
  print "trained prediciton"
  print (predictEpoch trainedWeights inputs)
  print "Validate"
  print (validate weights inputs labels)