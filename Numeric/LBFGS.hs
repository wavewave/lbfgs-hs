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
                      ProgressFun, lbfgs) where

import Data.Array.Storable (StorableArray,
                            unsafeForeignPtrToStorableArray)
import Foreign.C.Types (CDouble, CInt)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable(..), peek, poke, sizeOf)

import qualified Numeric.LBFGS.Raw as R
import Numeric.LBFGS.Raw (CEvaluateFun, CProgressFun, CLBFGSParameter(..),
                          defaultCParam,
                          c_lbfgs_malloc, c_lbfgs_free,
                          c_lbfgs_evaluate_t_wrap, c_lbfgs_progress_t_wrap,
                          c_lbfgs
                         )

-- |
-- Various line search algorithms. Wolfe backtracking algorithms require
-- a coefficient.
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
mergeLineSearchAlgorithm p (BacktrackingWolfe c) =
    p { R.linesearch = R.backtrackingWolfe,
        R.wolfe      = realToFrac c }
mergeLineSearchAlgorithm p (BacktrackingStrongWolfe c) =
    p { R.linesearch = R.backtrackingStrongWolfe,
        R.wolfe      = realToFrac c }

withParam :: LineSearchAlgorithm -> CLBFGSParameter
withParam lineSearch =
    mergeLineSearchAlgorithm defaultCParam lineSearch

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
vectorToList cn p = vectorToList_ p (cDoublePlusPtr p n) []
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
    -> StorableArray Int CDouble -- ^ Current variables
    -> StorableArray Int CDouble -- ^ Gradients
    -> CInt                      -- ^ Number of variables
    -> CDouble                   -- ^ Step of the line search algorithm
    -> IO (CDouble)              -- ^ Value of the objective function

wrapEvaluateFun :: (Storable a) => EvaluateFun a -> Ptr a -> Ptr CDouble ->
                   Ptr CDouble -> CInt -> CDouble -> IO (CDouble)
wrapEvaluateFun fun inst x g n step = do
  let nInt = fromIntegral n
  instV <- peek inst
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
    -> StorableArray Int CDouble -- ^ Variables
    -> StorableArray Int CDouble -- ^ Gradients
    -> CDouble                   -- ^ Value of the objective function
    -> CDouble                   -- ^ Euclidean norm of the variables
    -> CDouble                   -- ^ Eucledian norm of the gradients
    -> CDouble                   -- ^ Step of the line search algorithm
    -> CInt                      -- ^ Number of variables
    -> CInt                      -- ^ Iteration count
    -> CInt                      -- ^ Number of evaluations for this iteration
    -> IO (CInt)                 -- ^ Return zero to continue the evaluation,
                                 --   non-zero otherwise

wrapProgressFun :: (Storable a) => ProgressFun a -> Ptr a -> Ptr CDouble ->
                   Ptr CDouble-> CDouble -> CDouble -> CDouble -> CDouble ->
                   CInt -> CInt -> CInt -> IO (CInt)
wrapProgressFun fun inst x g fx xn gn step n k ls = do
  let nInt = fromIntegral n
  instV <- peek inst
  xFp <- newForeignPtr_ x
  xArr <- unsafeForeignPtrToStorableArray xFp (0, nInt - 1)
  gFp <- newForeignPtr_ g
  gArr <- unsafeForeignPtrToStorableArray gFp (0, nInt - 1)
  fun instV xArr gArr fx xn gn step n k ls

-- |
-- Start a L-BFGS optimization. The initial variables should be
-- provided as a list of doubles.
lbfgs :: (Storable a) =>
         LineSearchAlgorithm -- ^ The line search algorithm
      -> EvaluateFun a       -- ^ Objective function
      -> ProgressFun a       -- ^ Progress report function
      -> a                   -- ^ Instance data
      -> [Double]            -- ^ Initial variable values
      -> IO([Double])        -- ^ Variable values
lbfgs ls evalFun progressFun inst p = lbfgs_ ls (wrapEvaluateFun evalFun)
                                 (wrapProgressFun progressFun) inst p

lbfgs_ :: (Storable a) => LineSearchAlgorithm -> CEvaluateFun a ->
          CProgressFun a -> a -> [Double] -> IO([Double])
lbfgs_ ls evalFun progressFun inst p = do
  (n, pVec) <- listToVector p
  let param = withParam ls
  instP <- malloc
  poke instP inst
  paramP <- malloc
  poke paramP param
  evalW <- c_lbfgs_evaluate_t_wrap evalFun
  progressW <- c_lbfgs_progress_t_wrap progressFun
  _ <- c_lbfgs n pVec nullPtr evalW progressW instP paramP
  freeHaskellFunPtr progressW
  freeHaskellFunPtr evalW
  free paramP
  free instP
  freeVector pVec
  vectorToList n pVec
