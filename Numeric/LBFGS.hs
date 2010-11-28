-- |
-- Module      : Numeric.LBFGS
-- Copyright   : (c) 2010 Daniël de Kok
-- License     : Apache 2
--
--
-- Maintainer  : Daniël de Kok <me@danieldk.eu>
-- Stability   : experimental
--
-- Binding for the liblbfgs library, much implements the Limited-memory
-- Broyden-Fletcher-Goldfarb-Shanno (L-BFGS) method for solving
-- unconstrained minimization problems. The original C library is
-- available from:
--
-- <http://www.chokkan.org/software/liblbfgs/>

module Numeric.LBFGS (LineSearchAlgorithm(..), EvaluateFun,
                      ProgressFun, LBFGSParameters(..), LBFGSResult(..),
                      lbfgs) where

import Data.Array.Storable (StorableArray,
                            unsafeForeignPtrToStorableArray)
import Data.Maybe
import Foreign.C.Types (CDouble, CInt)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullPtr, plusPtr)
import Foreign.StablePtr (StablePtr, deRefStablePtr, newStablePtr,
                                   freeStablePtr)
import Foreign.Storable (Storable(..), peek, poke, sizeOf)

import qualified Numeric.LBFGS.Raw as R
import Numeric.LBFGS.Raw (CEvaluateFun, CProgressFun, CLBFGSParameter(..),
                          defaultCParam, CLBFGSResult(..),
                          c_lbfgs_malloc, c_lbfgs_free,
                          c_lbfgs_evaluate_t_wrap, c_lbfgs_progress_t_wrap,
                          c_lbfgs
                         )

-- |
-- Parameters for the LBFGS minimization.
data LBFGSParameters = LBFGSParameters LineSearchAlgorithm L1NormCoefficient

-- | Coefficient for the L1 norm of variables.
type L1NormCoefficient = Maybe Double

-- |
-- Various line search algorithms. Wolfe backtracking algorithms require
-- a coefficient.
data LineSearchAlgorithm = DefaultLineSearch
                         | MoreThuente
                         | BacktrackingArmijo
                         | Backtracking
                         | BacktrackingWolfe       {coeff :: Double }
                         | BacktrackingStrongWolfe {coeff :: Double }

mergeLineSearchAlgorithm :: LineSearchAlgorithm -> CLBFGSParameter ->
                            CLBFGSParameter
mergeLineSearchAlgorithm DefaultLineSearch p =
    p {R.linesearch = R.defaultLineSearch}
mergeLineSearchAlgorithm MoreThuente p =
    p { R.linesearch = R.moreThuente }
mergeLineSearchAlgorithm BacktrackingArmijo p =
    p { R.linesearch = R.backtrackingArmijo }
mergeLineSearchAlgorithm Backtracking p =
    p { R.linesearch = R.backtracking }
mergeLineSearchAlgorithm (BacktrackingWolfe c) p =
    p { R.linesearch = R.backtrackingWolfe,
        R.wolfe      = realToFrac c }
mergeLineSearchAlgorithm (BacktrackingStrongWolfe c) p =
    p { R.linesearch = R.backtrackingStrongWolfe,
        R.wolfe      = realToFrac c }

mergeL1NormCoefficient :: L1NormCoefficient -> CInt -> CLBFGSParameter ->
                          CLBFGSParameter
mergeL1NormCoefficient Nothing _ p = p
mergeL1NormCoefficient (Just l1) n p =
    p { R.linesearch        = R.backtracking,
        R.orthantwise_c     = realToFrac l1,
        R.orthantwise_start = 0,
        R.orthantwise_end   = n - 1 }

withParam :: LBFGSParameters -> CInt -> CLBFGSParameter
withParam (LBFGSParameters lineSearch l1NormCoeff) n =
    mergeL1NormCoefficient l1NormCoeff n $ (mergeLineSearchAlgorithm lineSearch)
                          defaultCParam 


data LBFGSResult
    = Success
    | Stop
    | AlreadyMinimized
    | UnknownError
    | LogicError
    | OutOfMemory
    | Canceled
    | InvalidN
    | InvalidNSSE
    | InvalidXSSE
    | InvalidEpsilon
    | InvalidTestPeriod
    | InvalidDelta
    | InvalidLineSearch
    | InvalidMinStep
    | InvalidMaxStep
    | InvalidFtol
    | InvalidWolfe
    | InvalidGtol
    | InvalidXtol
    | InvalidMaxLineSearch
    | InvalidOrthantwise
    | InvalidOrthantwiseStart
    | InvalidOrthantwiseEnd
    | OutOfInterval
    | IncorrectTMinMax
    | RoundingError
    | MinimumStep
    | MaximumStep
    | MaximumLineSearch
    | MaximumIteration
    | WidthTooSmall
    | InvalidParameters
    | IncreaseGradient
    deriving (Eq, Show)

deriveResult :: CLBFGSResult -> LBFGSResult
deriveResult r
    | r == R.lbfgsSuccess = Success
    | r == R.lbfgsStop = Stop
    | r == R.lbfgsAlreadyMinimized = AlreadyMinimized
    | r == R.lbfgserrUnknownerror = UnknownError
    | r == R.lbfgserrLogicerror = LogicError
    | r == R.lbfgserrOutofmemory = OutOfMemory
    | r == R.lbfgserrCanceled = Canceled
    | r == R.lbfgserrInvalidN = InvalidN
    | r == R.lbfgserrInvalidNSse = InvalidNSSE
    | r == R.lbfgserrInvalidXSse = InvalidXSSE
    | r == R.lbfgserrInvalidEpsilon = InvalidEpsilon
    | r == R.lbfgserrInvalidTestperiod = InvalidTestPeriod
    | r == R.lbfgserrInvalidDelta = InvalidDelta
    | r == R.lbfgserrInvalidLinesearch = InvalidLineSearch
    | r == R.lbfgserrInvalidMinstep = InvalidMinStep
    | r == R.lbfgserrInvalidMaxstep = InvalidMaxStep
    | r == R.lbfgserrInvalidFtol = InvalidFtol
    | r == R.lbfgserrInvalidWolfe = InvalidWolfe
    | r == R.lbfgserrInvalidGtol = InvalidGtol
    | r == R.lbfgserrInvalidXtol = InvalidXtol
    | r == R.lbfgserrInvalidMaxlinesearch = InvalidMaxLineSearch
    | r == R.lbfgserrInvalidOrthantwise = InvalidOrthantwise
    | r == R.lbfgserrInvalidOrthantwiseStart = InvalidOrthantwiseStart
    | r == R.lbfgserrInvalidOrthantwiseEnd = InvalidOrthantwiseEnd
    | r == R.lbfgserrOutofinterval = OutOfInterval
    | r == R.lbfgserrIncorrectTminmax = IncorrectTMinMax
    | r == R.lbfgserrRoundingError = RoundingError
    | r == R.lbfgserrMinimumstep = MinimumStep
    | r == R.lbfgserrMaximumstep = MaximumStep
    | r == R.lbfgserrMaximumlinesearch = MaximumLineSearch
    | r == R.lbfgserrMaximumiteration = MaximumIteration
    | r == R.lbfgserrWidthtoosmall = WidthTooSmall
    | r == R.lbfgserrInvalidparameters = InvalidParameters
    | r == R.lbfgserrIncreasegradient = IncreaseGradient

cDoublePlusPtr :: Ptr CDouble -> Int -> Ptr CDouble
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
vectorToList cn p = vectorToList_ p (cDoublePlusPtr p (n - 1)) []
    where n = fromIntegral cn

vectorToList_ :: Ptr CDouble -> Ptr CDouble -> [Double] -> IO ([Double])
vectorToList_ pStart pCur l
    | pCur >= pStart = do
  cval <- peek pCur
  let val = realToFrac cval
  vectorToList_ pStart (cDoublePlusPtr pCur (-1)) (val:l)
    | otherwise = return l


-- |
-- Type signature for the objective function and gradient evaluations.
type EvaluateFun a =
    a                            -- ^ Instance data
    -> StorableArray Int CDouble -- ^ Current variables (should not be
                                 --   modified by the function)
    -> StorableArray Int CDouble -- ^ Gradients
    -> CInt                      -- ^ Number of variables
    -> CDouble                   -- ^ Step of the line search algorithm
    -> IO (CDouble)              -- ^ Value of the objective function

wrapEvaluateFun :: EvaluateFun a -> StablePtr a -> Ptr CDouble ->
                   Ptr CDouble -> CInt -> CDouble -> IO (CDouble)
wrapEvaluateFun fun inst x g n step = do
  let nInt = fromIntegral n
  instV <- deRefStablePtr inst
  xFp <- newForeignPtr_ x
  xArr <- unsafeForeignPtrToStorableArray xFp (0, nInt - 1)
  gFp <- newForeignPtr_ g
  gArr <- unsafeForeignPtrToStorableArray gFp (0, nInt - 1)
  fun instV xArr gArr n step

-- |
-- Type signature for a function reporting on the progress of the
-- optimization.
type ProgressFun a =
    a                            -- ^ Instance data
    -> StorableArray Int CDouble -- ^ Variables (should not be modified
                                 --   by the function)
    -> StorableArray Int CDouble -- ^ Gradients (should not be modified
                                 --   by the function)
    -> CDouble                   -- ^ Value of the objective function
    -> CDouble                   -- ^ Euclidean norm of the variables
    -> CDouble                   -- ^ Eucledian norm of the gradients
    -> CDouble                   -- ^ Step of the line search algorithm
    -> CInt                      -- ^ Number of variables
    -> CInt                      -- ^ Iteration count
    -> CInt                      -- ^ Number of evaluations for this iteration
    -> IO (CInt)                 -- ^ Return zero to continue the evaluation,
                                 --   non-zero otherwise

wrapProgressFun :: ProgressFun a -> StablePtr a -> Ptr CDouble ->
                   Ptr CDouble-> CDouble -> CDouble -> CDouble -> CDouble ->
                   CInt -> CInt -> CInt -> IO (CInt)
wrapProgressFun fun inst x g fx xn gn step n k ls = do
  let nInt = fromIntegral n
  instV <- deRefStablePtr inst
  xFp <- newForeignPtr_ x
  xArr <- unsafeForeignPtrToStorableArray xFp (0, nInt - 1)
  gFp <- newForeignPtr_ g
  gArr <- unsafeForeignPtrToStorableArray gFp (0, nInt - 1)
  fun instV xArr gArr fx xn gn step n k ls

-- |
-- Start a L-BFGS optimization. The initial variables should be
-- provided as a list of doubles.
lbfgs :: LBFGSParameters           -- ^ Parameters
      -> EvaluateFun a             -- ^ Objective function
      -> ProgressFun a             -- ^ Progress report function
      -> a                         -- ^ Instance data
      -> [Double]                  -- ^ Initial variable values
      -> IO(LBFGSResult, [Double]) -- ^ Result and variable values
lbfgs lbfgsParams evalFun progressFun inst p = lbfgs_ lbfgsParams
                                               (wrapEvaluateFun evalFun)
                                               (wrapProgressFun progressFun) inst p

lbfgs_ :: LBFGSParameters -> CEvaluateFun a -> CProgressFun a -> a ->
          [Double] -> IO(LBFGSResult, [Double])
lbfgs_ lbfgsParams evalFun progressFun inst p = do
  (n, pVec) <- listToVector p
  let param = withParam lbfgsParams n
  instP <- newStablePtr inst
  paramP <- malloc
  poke paramP param
  evalW <- c_lbfgs_evaluate_t_wrap evalFun
  progressW <- c_lbfgs_progress_t_wrap progressFun
  r <- c_lbfgs n pVec nullPtr evalW progressW instP paramP
  freeHaskellFunPtr progressW
  freeHaskellFunPtr evalW
  free paramP
  freeStablePtr instP
  freeVector pVec
  rl <- vectorToList n pVec
  return (deriveResult $ CLBFGSResult r, rl)
