module Main where

-- import Lib (train)

-- import Data.Binary.Strict.Get as BinGet

-- import Lib (train)

-- import Data.Binary.Strict.Get as BinGet
-- import Control.Monad (liftM2, when)
-- import qualified Data.ByteString.Lazy as BL
-- import Data.Word

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

-- readHeader :: BinGet.Get (Word8, Word8)
-- readHeader = do
--   _ <- getword16be
--   dataType <- getWord8be
--   dimensions <- getWord8be
--   return (dataType, dimensions)

main :: IO ()
main = do
  -- contents <- BL.readFile "./data/train-labels-idx1-ubyte"
  -- BL.writeFile "test_write" $ runGet readHeader contents

  -- print $ outer [1, 2] [3, 4]

  let inputs = [[0, 0, 1], [0, 1, 1], [1, 0, 1], [1, 1, 1]]
  let labels = [0, 0, 0, 1]
  let weights = [0 ..]
  print (predictEpoch weights inputs)
  let trainedWeights = train 100 inputs labels weights
  let trained = predictEpoch
  print (predictEpoch trainedWeights inputs)
  print trainedWeights
