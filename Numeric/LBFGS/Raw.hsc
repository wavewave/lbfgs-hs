{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving, 
  EmptyDataDecls #-}

#include "lbfgs.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

module Numeric.LBFGS.Raw (CLineSearchAlgorithm(..), CLBFGSParameter(..),
                          defaultCParam, c_lbfgs, c_lbfgs_malloc,
                          c_lbfgs_free, lbfgs_evaluate_t_wrap,
                          lbfgs_progress_t_wrap,

                          defaultLineSearch, moreThuente, backtrackingArmijo,
                          backtracking, backtrackingWolfe,
                          backtrackingStrongWolfe

) where

import Foreign.Storable (Storable(..))
import Foreign.C.Types (CDouble, CInt)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr)

newtype CLineSearchAlgorithm =
    CLineSearchAlgorithm { unCLineSearchAlgorithm :: CInt }
    deriving (Storable, Show)

#{enum CLineSearchAlgorithm, CLineSearchAlgorithm,
  defaultLineSearch = LBFGS_LINESEARCH_DEFAULT,
  moreThuente = LBFGS_LINESEARCH_MORETHUENTE,
  backtrackingArmijo = LBFGS_LINESEARCH_BACKTRACKING_ARMIJO,
  backtracking = LBFGS_LINESEARCH_BACKTRACKING,
  backtrackingWolfe = LBFGS_LINESEARCH_BACKTRACKING_WOLFE,
  backtrackingStrongWolfe = LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE
}

data CLBFGSParameter = CLBFGSParameter {
      m :: CInt,
      epsilon :: CDouble,
      past :: CInt,
      delta :: CDouble,
      max_iterations :: CInt,
      linesearch :: CLineSearchAlgorithm,
      max_linesearch :: CInt,
      min_step :: CDouble,
      max_step :: CDouble,
      ftol :: CDouble,
      wolfe :: CDouble,
      gtol :: CDouble,
      xtol :: CDouble,
      orthantwise_c :: CDouble,
      orthantwise_start :: CDouble,
      orthantwise_end :: CDouble
} deriving Show

defaultCParam = CLBFGSParameter 6 1e-5 0 1e-5 0 defaultLineSearch 40 1e-20
                1e20 1e-4 0.9 0.9 1.0e-16 0.0 0.0 (-1.0)


instance Storable CLBFGSParameter where
    sizeOf _ = #{size lbfgs_parameter_t}
    alignment _ = #{alignment lbfgs_parameter_t}
    peek ptr = do
      m                 <- (#peek lbfgs_parameter_t, m) ptr
      epsilon           <- (#peek lbfgs_parameter_t, epsilon) ptr
      past              <- (#peek lbfgs_parameter_t, past) ptr
      delta             <- (#peek lbfgs_parameter_t, delta) ptr
      max_iterations    <- (#peek lbfgs_parameter_t, max_iterations) ptr
      linesearch        <- (#peek lbfgs_parameter_t, linesearch) ptr
      max_linesearch    <- (#peek lbfgs_parameter_t, max_linesearch) ptr
      min_step          <- (#peek lbfgs_parameter_t, min_step) ptr
      max_step          <- (#peek lbfgs_parameter_t, max_step) ptr
      ftol              <- (#peek lbfgs_parameter_t, ftol) ptr
      wolfe             <- (#peek lbfgs_parameter_t, wolfe) ptr
      gtol              <- (#peek lbfgs_parameter_t, gtol) ptr
      xtol              <- (#peek lbfgs_parameter_t, xtol) ptr
      orthantwise_c     <- (#peek lbfgs_parameter_t, orthantwise_c) ptr
      orthantwise_start <- (#peek lbfgs_parameter_t, orthantwise_start) ptr
      orthantwise_end   <- (#peek lbfgs_parameter_t, orthantwise_end) ptr
      return $ CLBFGSParameter m epsilon past delta max_iterations
             linesearch max_linesearch min_step max_step
             ftol wolfe gtol xtol orthantwise_c
             orthantwise_start orthantwise_end
    poke ptr (CLBFGSParameter m epsilon past delta max_iterations
                              linesearch max_linesearch min_step max_step
                              ftol wolfe gtol xtol orthantwise_c
                              orthantwise_start orthantwise_end
             ) = do
      (#poke lbfgs_parameter_t, m) ptr m
      (#poke lbfgs_parameter_t, epsilon) ptr epsilon
      (#poke lbfgs_parameter_t, past) ptr past
      (#poke lbfgs_parameter_t, delta) ptr delta
      (#poke lbfgs_parameter_t, max_iterations) ptr max_iterations
      (#poke lbfgs_parameter_t, linesearch) ptr linesearch
      (#poke lbfgs_parameter_t, max_linesearch) ptr max_linesearch
      (#poke lbfgs_parameter_t, min_step) ptr min_step
      (#poke lbfgs_parameter_t, max_step) ptr max_step
      (#poke lbfgs_parameter_t, ftol) ptr ftol
      (#poke lbfgs_parameter_t, wolfe) ptr wolfe
      (#poke lbfgs_parameter_t, gtol) ptr gtol
      (#poke lbfgs_parameter_t, xtol) ptr xtol
      (#poke lbfgs_parameter_t, orthantwise_c) ptr orthantwise_c
      (#poke lbfgs_parameter_t, orthantwise_start) ptr orthantwise_start
      (#poke lbfgs_parameter_t, orthantwise_end) ptr orthantwise_end

type EvaluateFun a = (Ptr a -> Ptr CDouble -> Ptr CDouble -> CInt ->
                      CDouble -> IO (CDouble))

type ProgressFun a = (Ptr a -> Ptr CDouble -> Ptr CDouble -> CDouble ->
                      CDouble -> CDouble -> CDouble -> CInt -> CInt ->
                      CInt -> IO (CInt))

foreign import ccall "wrapper"
        lbfgs_evaluate_t_wrap :: EvaluateFun a -> IO (FunPtr (EvaluateFun a))

foreign import ccall "wrapper"
        lbfgs_progress_t_wrap :: ProgressFun a -> IO (FunPtr (ProgressFun a))

foreign import ccall safe "lbfgs.h lbfgs" c_lbfgs ::
    CInt -> Ptr CDouble -> Ptr CDouble -> FunPtr (EvaluateFun a) ->
    FunPtr (ProgressFun a) -> Ptr a -> Ptr (CLBFGSParameter) -> IO (CInt)

foreign import ccall unsafe "lbfgs.h lbfgs_malloc" c_lbfgs_malloc ::
    CInt -> IO (Ptr CDouble)

foreign import ccall unsafe "lbfgs.h lbfgs_free" c_lbfgs_free ::
    Ptr CDouble -> IO ()

