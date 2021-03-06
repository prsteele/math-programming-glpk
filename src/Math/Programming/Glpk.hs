-- | A <https://www.gnu.org/software/glpk/ GLPK> backend to the 'Math.Programming' library.
--
-- This package allows both linear and mixed-integer programs to be solved.
module Math.Programming.Glpk
  ( Glpk
  , runGlpk
  , writeFormulation
  -- ** Controlling GLPK behavior
  -- $settings
  , GlpkEnv
  , GlpkError
  )
  where

import           Math.Programming.Glpk.Internal

-- $settings
--
-- See the 'Math.Programming.Glpk.Header' package for Haskell wrappers
-- for all low-level GLPK operations.
