import Data.Vector.Storable.Mutable (IOVector,MVector(..))
import Data.Vector.Storable ((!))
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as M
import Foreign.Storable.Tuple

import Numeric.LBFGS.Vector
         (LineSearchAlgorithm(..), LBFGSParameters(..),
          LBFGSResult, lbfgs)

import Foreign.C.Types (CDouble, CInt)

import Control.Lens (view, _1,_2,_3 )

--
-- objective function here is f(x) = Sum_(k=0)^50 ( (1-x[2k])^2 + 10^2 (x[2k+1] - x[2k]^2)^2 )
--
eval :: Double -> IOVector CDouble -> IOVector CDouble -> CInt -> CDouble -> IO (CDouble)
eval inst x g _n step = do
  x' <- VS.unsafeFreeze x
  let n = VS.length x'
      x'xy = VS.generate (n `div` 2) (\k -> (x' ! (2*k), x' ! (2*k+1) ))
      irst = flip VS.map x'xy $ \(x,y) ->
        let t1 = 1.0-x
            t2 = 10.0*(y-x*x)
            fxy = t1*t1+t2*t2
            ngrad = 20.0*t2
            gradx = -2.0*(x*ngrad+t1)
            grady = ngrad
        in (fxy,gradx,grady)
      f = VS.sum . VS.map (view _1) $ irst
      g' = VS.generate n (\i -> let (k,m) = i `divMod` 2 in if m == 0 then view _2 (irst ! k) else view _3 (irst ! k))
  VS.copy g g'
  return f

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

