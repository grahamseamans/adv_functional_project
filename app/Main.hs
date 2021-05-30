module Main where

-- import Lib (train)
import Control.Monad (liftM2)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Word
import Text.CSV

type Vect = [Float]

predict ::
  Vect -> -- Weights
  Vect -> -- Input Vector
  Float -- Guess
predict w x
  | dot w x > 0 = 1
  | otherwise = 0

predictEpoch ::
  Vect -> -- x - Weights
  [Vect] -> -- curried - An epoch of input vectors
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

trainEpoch ::
  [Vect] -> -- x - An epoch of inputs
  Vect -> -- l - An epoch of labels
  Vect -> -- w - Weights
  Vect -- Weights
trainEpoch [] [] w = w
trainEpoch (x : xs) (l : ls) w = trainEpoch xs ls (stocTrain w x l)

train ::
  Int -> -- t - number of epochs to train on
  [Vect] -> -- x - An epoch of inputs
  Vect -> -- l - An epoch of labels
  Vect -> -- w - Weights
  Vect -- Weights
train t xs ls w
  | t == 0 = w
  | otherwise = train (t - 1) xs ls (trainEpoch xs ls w)

dot :: Vect -> Vect -> Float
dot x y = sum (zipWith (*) x y)

elemSubtract :: Vect -> Vect -> Vect
elemSubtract = zipWith (-)

elemAdd :: Vect -> Vect -> Vect
elemAdd = zipWith (+)

-- outer :: Vect -> Vect -> Vect
-- outer = liftM2 (*)

-- https://hackage.haskell.org/package/linear-1.21.5/docs/src/Linear.Vector.html#outer
outer :: Vect -> Vect -> [Vect]
outer a b = fmap (\x -> fmap (x *) b) a

-- csvToLists :: [[String]] -> [Vect]
-- csvToLists s = trainedWeights
--   where
--     (labels, inputs) = labelInputSplit (map (map read) s)
--     trainedWeights = train 100 inputs labels [[0 ..]]

-- labelInputSplit :: [[Float]] -> ([Float], [[Float]])
-- labelInputSplit xs = (heads xs, tails xs)

-- heads :: [[Float]] -> [Float]
-- heads (x : xs) = head x : heads xs

-- tails :: [[Float]] -> [[Float]]
-- tails (x : xs) = tail x : tails xs

main :: IO ()
main = do
  -- file <- decodeIDXFile "./data/train-labels-idx1-ubyte"
  -- case file of
  --   Nothing -> print "error"
  --   Just idx -> print idx

  -- test_csv <- parseCSVFromFile "test_read.csv"
  -- case test_csv of
  --   Left err -> print err
  --   Right csv -> print (csvToLists csv)

  -- extractColumn :: Read t => CSV -> Int -> [t]
  -- extractColumn csv n =
  --   [ read (record !! n) | record <- csv, length record > n, record /= [""]
  --   ]

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
