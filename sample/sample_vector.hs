import Data.Vector.Storable.Mutable (IOVector,MVector(..))
import qualified Data.Vector.Storable.Mutable as M
import Numeric.LBFGS.Vector
         (LineSearchAlgorithm(..), LBFGSParameters(..),
          LBFGSResult, lbfgs)

import Foreign.C.Types (CDouble, CInt)

--
-- data StorableArray (index type) (value type)
--
-- StorableArray is mutable array.
-- mutable read and write can be done using readArray, writeArray.
-- Note that readArray/writeArray are IO actions.
--

--
-- objective function here is f(x) = Sum_(k=0)^50 ( (1-x[2k])^2 + 10^2 (x[2k+1] - x[2k]^2)^2 )
--
eval :: Double -> IOVector CDouble -> IOVector CDouble -> CInt -> CDouble -> IO (CDouble)
eval inst x g n step = eval_ inst x g n step 0.0 {- initial value of sum -} 0 {- initial index i -} 

eval_ :: Double                    -- ^ inst ; internal state
      -> IOVector CDouble -- ^ x: domain vector 
      -> IOVector CDouble -- ^ gradient: Del f
      -> CInt                      -- ^ n ( dim)  
      -> CDouble                   -- ^ step
      --------------------------------  eval up to here 
      -> CDouble                   -- ^ fx (sum ; initial = 0)
      -> CInt                      -- ^ i (index; initial = 0)
      -> IO (CDouble)              -- ^ f(x) 
eval_ inst x g n step fx curN
  | curN >= n = return fx                                -- if i >= dim => finish
  | otherwise = do                                       -- if i < dim
      let nInt = fromIntegral curN                       -- curN = nInt = 2k
      val <- M.read x nInt                               -- val = x[2k]
      nextVal <- M.read x (nInt + 1)                     -- nextVal = x[2k+1]
      let t1 = 1.0 - val
          t2 = 10.0*(nextVal - val*val)
          nFx = fx + (t1*t1 + t2*t2)
          nextGrad = 20.0*t2
      M.write g (nInt + 1) nextGrad                   -- g[2k+1] = nextGrad
      M.write g nInt $ (-2.0) * (val * nextGrad + t1) -- g[2k] = -2*(val*nextGrad + t1) 
      eval_ inst x g n step nFx (curN + 2)               -- recursion corresponding to (do ... while) 

--
-- progress is called at each step.
--

progress :: a -> IOVector CDouble -> IOVector CDouble
         -> CDouble -> CDouble -> CDouble -> CDouble -> CInt -> CInt 
         -> CInt -> IO (CInt)
progress _ x _ fx _ _ _ _ k _ = do
    x0 <- M.read x 0
    putStrLn $ "Iteration " ++ show k ++ " :"
    putStrLn $ "fx = " ++ show fx ++ ", x[0] = " ++ show x0
    return 0

--
-- initial x_vector (100-dimensional)
--
-- (-1.2,1.0,-1.2,1.0,-1.2,1.0,...,-1.2,1.0)
--

test_init :: [Double]
test_init = concat $ take 50 $ repeat [-1.2, 1.0]


--
-- main driver function
--
test :: IO (LBFGSResult, [Double])
test = do
    putStrLn "--- Starting optimization ---"
    r <- lbfgs params eval progress 0.0 test_init
    putStrLn "--- Done ---"
    return r
  where
    --
    -- we need to clarify what these mean.
    --
    params = LBFGSParameters
             { lbfgsPast              = Nothing
             , lbfgsDelta             = 0
             , lbfgsLineSearch        = DefaultLineSearch
             , lbfgsL1NormCoefficient = Nothing
             }

main = do
    (r,_) <- test
    putStrLn $ "Result: " ++ show r

