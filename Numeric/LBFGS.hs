module Numeric.LBFGS (LineSearchAlgorithm(..)) where

import Numeric.LBFGS.Raw as R
import Numeric.LBFGS.Raw (CLBFGSParameter(..), CLineSearchAlgorithm(..))

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

