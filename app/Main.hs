module Main where

-- import Lib (train)
import Control.Monad (liftM2)
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.Vector as V
import Data.Word
import Text.CSV

type Vect = [Double]

predict ::
  Vect -> -- curried - Weights
  Vect -> -- curried - Input Vector
  Double -- Guess
predict = dot

predictEpoch ::
  Vect -> -- x - Weights
  [Vect] -> -- curried - An epoch of input vectors
  [Double] -- A guess for each input in the epoch
predictEpoch w = map (predict w)

stocTrain ::
  Vect -> -- w - Weights
  Vect -> -- x - Single input vector
  Double -> -- l - label
  Vect -- Trained Weights
stocTrain w x l = elemSubtract w (map (0.1 * error *) x)
  where
    error = predict w x - l

trainEpoch ::
  [Vect] -> -- x - An epoch of inputs
  Vect -> -- l - An epoch of labels
  Vect -> -- w - Weights
  Vect -- Weights
trainEpoch (x : xs) (l : ls) w = trainEpoch xs ls (stocTrain w x l)
trainEpoch [] [] w = w

train ::
  Int -> -- t - number of epochs to train on
  [Vect] -> -- x - An epoch of inputs
  Vect -> -- l - An epoch of labels
  Vect -> -- w - Weights
  Vect -- Weights
train t xs ls w
  | t == 0 = w
  | otherwise = train (t - 1) xs ls (trainEpoch xs ls w)

dot :: Vect -> Vect -> Double
dot x y = sum (zipWith (*) x y)

elemSubtract :: Vect -> Vect -> Vect
elemSubtract = zipWith (-)

elemAdd :: Vect -> Vect -> Vect
elemAdd = zipWith (+)

-- csvToTrainedWeights :: [[String]] -> [Vect]
-- csvToTrainedWeights :: [[String]] -> [Double]
csvToTrainedWeights :: [[String]] -> [Double]
csvToTrainedWeights s = trainedWeights
  where
    (labels, unscaledInputs) = labelInputSplit (map (map read) (init s))
    trainedWeights = train 1 (scaleInput unscaledInputs) labels [0 ..]

scaleInput :: [[Double]] -> [[Double]]
scaleInput = map (map (/ 255))

labelInputSplit :: [[Double]] -> ([Double], [[Double]])
labelInputSplit xs = (head t, transpose (tail t))
  where
    t = transpose xs

main :: IO ()
main = do
  -- test_csv <- parseCSVFromFile "./data/mnist_test.csv"
  -- case test_csv of
  --   Left err -> print err
  --   Right csv -> print (csvToTrainedWeights csv)

  -- contents <- BL.readFile "./data/train-labels-idx1-ubyte"
  -- let header = BL.take 4 contents
  -- BL.writeFile "test_write" header

  let inputs = [[0, 0, 1], [0, 1, 1], [1, 0, 1], [1, 1, 1]]
  let labels = [0, 0, 0, 1]
  let weights = [0 ..]
  print (predictEpoch weights inputs)
  let trainedWeights = train 100 inputs labels weights
  let trained = predictEpoch
  print (predictEpoch trainedWeights inputs)
  print trainedWeights
