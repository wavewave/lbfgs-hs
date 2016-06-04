-- |
-- Module      : Numeric.LBFGS.Types
-- Copyright   : (c) 2010 Daniël de Kok, 2016 Ian-Woo.Kim
-- License     : Apache 2
--
--
-- Maintainer  : Daniël de Kok <me@danieldk.eu>
-- Stability   : experimental
--

module Numeric.LBFGS.Types
( LineSearchAlgorithm(..)
, LBFGSParameters(..)
, LBFGSResult(..)
, L1NormCoefficient
) where




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

-- |
-- Parameters for the LBFGS minimization.
data LBFGSParameters = LBFGSParameters {
      lbfgsPast              :: Maybe Int,
      lbfgsDelta             :: Double,
      lbfgsLineSearch        :: LineSearchAlgorithm,
      lbfgsL1NormCoefficient :: L1NormCoefficient
}

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

