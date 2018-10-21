module ApiTests where

import Test.Tasty
import Test.Tasty.HUnit

import Math.Programming.Glpk
import Math.Programming.Tests

test_tree :: TestTree
test_tree = makeAllTests "GLPK" glpkRunner

glpkRunner :: Glpk () -> IO ()
glpkRunner program = do
  result <- runGlpk program
  case result of
    Left errorMsg -> assertFailure (show errorMsg)
    Right () -> return ()
  return ()
