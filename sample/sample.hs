-- LBFGS usage example, comparable to sample/sample.c in the liblbfgs
-- distribution.

import Data.Array.Storable (StorableArray, readArray, writeArray)
import Numeric.LBFGS (LineSearchAlgorithm(..), lbfgs)
import Foreign.C.Types (CDouble, CInt)

eval :: Double -> StorableArray Int CDouble -> StorableArray Int CDouble ->
        CInt -> CDouble -> IO (CDouble)
eval inst x g n step = eval_ inst x g n step 0.0 0

eval_ :: Double -> StorableArray Int CDouble -> StorableArray Int CDouble ->
        CInt -> CDouble -> CDouble -> CInt -> IO (CDouble)
eval_ inst x g n step fx curN
    | curN >= n = do return fx
    | otherwise = do
  let nInt = fromIntegral curN
  val <- readArray x nInt
  nextVal <- readArray x $ nInt + 1
  let t1 = 1.0 - val
  let t2 = 10.0 * (nextVal - val * val)
  let nFx = fx + (t1 * t1 + t2 * t2)
  let nextGrad = 20.0 * t2
  writeArray g (nInt + 1) nextGrad
  writeArray g nInt $ (-2.0) * (val * nextGrad + t1)
  eval_ inst x g n step nFx (curN + 2)

progress :: a -> StorableArray Int CDouble -> StorableArray Int CDouble ->
            CDouble -> CDouble -> CDouble -> CDouble -> CInt -> CInt ->
            CInt -> IO (CInt)
progress _ x _ fx _ _ _ _ k _ = do
  x0 <- readArray x 0
  putStr $ "Iteration " ++ show(k) ++ " :\n"
  putStr $ "fx = " ++ show(fx) ++ ", x[0] = " ++ show(x0) ++ "\n\n"
  return 0

test_init :: [Double]
test_init = concat $ take 50 $ repeat [-1.2, 1.0]

test :: IO ([Double])
test = do
  putStr "--- Starting optimization ---\n"
  p <- lbfgs DefaultLineSearch eval progress 0.0 test_init
  putStr "--- Done ---\n"
  return p

main :: IO ()
main = do
	_ <- test
        return ()
