-- |
-- Module      : Numeric.LBFGS.Types
-- Copyright   : (c) 2010 Daniël de Kok, 2016 Ian-Woo.Kim
-- License     : Apache 2
--
--
-- Maintainer  : Daniël de Kok <me@danieldk.eu>
-- Stability   : experimental
--

module Numeric.LBFGS.Internal where

import Foreign.C.Types (CDouble, CInt)
import qualified Numeric.LBFGS.Raw as R
import Numeric.LBFGS.Raw (CEvaluateFun, CProgressFun, CLBFGSParameter(..),
                          defaultCParam, CLBFGSResult(..),
                          c_lbfgs_malloc, c_lbfgs_free,
                          c_lbfgs_evaluate_t_wrap, c_lbfgs_progress_t_wrap,
                          c_lbfgs
                         )
import Numeric.LBFGS.Types

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

mergePast :: Maybe Int -> Double -> CLBFGSParameter -> CLBFGSParameter
mergePast Nothing           delta p = p { R.past = 0 }
mergePast (Just iterations) delta p = p {
                                        R.past  = fromIntegral iterations,
                                        R.delta = realToFrac delta
                                      }

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

