module Numeric.LBFGS (LineSearchAlgorithm(..), lbfgs) where

import Foreign.C.Types (CDouble, CInt)
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, sizeOf)

import qualified Numeric.LBFGS.Raw as R
import Numeric.LBFGS.Raw (CEvaluateFun, CProgressFun, CLBFGSParameter(..),
                          CLineSearchAlgorithm(..), defaultCParam,
                          c_lbfgs_malloc, c_lbfgs_free,
                          c_lbfgs_evaluate_t_wrap, c_lbfgs_progress_t_wrap,
                          c_lbfgs
                         )

data LineSearchAlgorithm = DefaultLineSearch
                         | MoreThuente
                         | BacktrackingArmijo
                         | Backtracking
                         | BacktrackingWolfe       {coeff :: Double }
                         | BacktrackingStrongWolfe {coeff :: Double }

mergeLineSearchAlgorithm :: CLBFGSParameter -> LineSearchAlgorithm ->
                            CLBFGSParameter
mergeLineSearchAlgorithm p DefaultLineSearch =
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
  copyList l v
  return (n, v)
    where n = fromIntegral . length $ l

copyList :: [Double] -> Ptr CDouble -> IO ()
copyList [] _ = return ()
copyList l p = do
  poke p $ realToFrac $ head l
  copyList (tail l) (cDoublePlusPtr p 1)


freeVector :: Ptr CDouble -> IO ()
freeVector = c_lbfgs_free

vectorToList :: CInt -> Ptr CDouble -> IO ([Double])
vectorToList cn p = vectorToList_ p (cDoublePlusPtr p n) []
    where n = fromIntegral cn

vectorToList_ :: Ptr CDouble -> Ptr CDouble -> [Double] -> IO ([Double])
vectorToList_ pStart pCur l
    | pCur >= pStart = do
  cval <- peek pCur
  let val = realToFrac cval
  vectorToList_ pStart (cDoublePlusPtr pCur (-1)) (val:l)
    | otherwise = return l

lbfgs :: LineSearchAlgorithm -> CEvaluateFun a -> CProgressFun a ->
         [Double] -> IO([Double])
lbfgs ls evalFun progressFun p = do
  (n, pVec) <- listToVector p
  let param = withParam ls
  paramP <- malloc
  poke paramP param
  evalW <- c_lbfgs_evaluate_t_wrap evalFun
  progressW <- c_lbfgs_progress_t_wrap progressFun
  r <- c_lbfgs n pVec nullPtr evalW progressW nullPtr paramP
  freeHaskellFunPtr progressW
  freeHaskellFunPtr evalW
  free paramP
  freeVector pVec
  vectorToList n pVec
