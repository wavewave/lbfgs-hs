module Numeric.LBFGS (LineSearchAlgorithm(..)) where

import Foreign.C.Types (CDouble, CInt)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, plusPtr)
import Foreign.Storable (poke, sizeOf)

import Numeric.LBFGS.Raw as R
import Numeric.LBFGS.Raw (CLBFGSParameter(..), CLineSearchAlgorithm(..),
                         c_lbfgs_malloc, c_lbfgs_free)

data LineSearchAlgorithm = DefaultLS
                         | MoreThuente
                         | BacktrackingArmijo
                         | Backtracking
                         | BacktrackingWolfe       {coeff :: Double }
                         | BacktrackingStrongWolfe {coeff :: Double }

mergeLineSearchAlgorithm :: CLBFGSParameter -> LineSearchAlgorithm ->
                            CLBFGSParameter
mergeLineSearchAlgorithm p DefaultLS =
    p {R.linesearch = R.defaultLineSearch}
mergeLineSearchAlgorithm p MoreThuente =
    p { R.linesearch = R.moreThuente }
mergeLineSearchAlgorithm p BacktrackingArmijo =
    p { R.linesearch = R.backtrackingArmijo }
mergeLineSearchAlgorithm p Backtracking =
    p { R.linesearch = R.backtracking }
mergeLineSearchAlgorithm p (BacktrackingWolfe coeff) =
    p { R.linesearch = R.backtrackingWolfe,
        R.wolfe      = realToFrac coeff }
mergeLineSearchAlgorithm p (BacktrackingStrongWolfe coeff) =
    p { R.linesearch = R.backtrackingStrongWolfe,
        R.wolfe      = realToFrac coeff }

withParam :: LineSearchAlgorithm -> CLBFGSParameter
withParam lineSearch =
    mergeLineSearchAlgorithm defaultCParam lineSearch

cDoublePlusPtr ptr n = plusPtr ptr (n * sizeOf (undefined :: CDouble))

listToVector :: [Double] -> IO (CInt, Ptr CDouble)
listToVector l = do
  v <- c_lbfgs_malloc n
  return (n, v)
    where n = fromIntegral . length $ l
          copyList [] _ = return ()
          copylist (x:xs) p = do
                   poke p $ realToFrac x
                   copyList xs (cDoublePlusPtr p 1)