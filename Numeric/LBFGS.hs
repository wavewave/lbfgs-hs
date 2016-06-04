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

import Data.Array.Storable (StorableArray)
import Data.Array.Unsafe (unsafeForeignPtrToStorableArray)
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
import Numeric.LBFGS.Types
--
import Numeric.LBFGS.Internal


withParam :: LBFGSParameters -> CInt -> CLBFGSParameter
withParam (LBFGSParameters past delta lineSearch l1NormCoeff) n =
    mergeL1NormCoefficient l1NormCoeff n $ (mergeLineSearchAlgorithm lineSearch)
                           $ mergePast past delta defaultCParam


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
  rl <- vectorToList n pVec
  freeVector pVec
  return (deriveResult $ CLBFGSResult r, rl)
