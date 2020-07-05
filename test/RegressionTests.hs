module RegressionTests where

import           Control.Monad.IO.Class

import           Test.Tasty
import           Test.Tasty.HUnit

import           Math.Programming
import           Math.Programming.Glpk

test_tree :: TestTree
test_tree = testGroup "Regression tests"
            [ testCase "Free variables (LP)" testFreeVariablesLP
            , testCase "Free variables (IP)" testFreeVariablesIP
            ]

assertFeasible :: SolutionStatus -> Glpk ()
assertFeasible result
  = liftIO $ case result of
      Error      -> assertFailure "Failed to solve program"
      Unbounded  -> assertFailure "Unbounded program"
      Infeasible -> assertFailure "Infeasible program"
      _          -> pure ()

assertRunGlpk :: Glpk a -> IO ()
assertRunGlpk program = do
  eResult <- runGlpk program
  case eResult of
    Left err -> assertFailure ("Error solving program: " <> show err)
    Right _  -> pure ()

testFreeVariablesLP :: IO ()
testFreeVariablesLP = assertRunGlpk $ do
  x <- free
  y <- free
  z <- free

  _ <- x @==# 0
  _ <- y @==# 3.1
  _ <- z @==# -3.1

  optimizeLP >>= assertFeasible

  vx <- getVariableValue x
  vy <- getVariableValue y
  vz <- getVariableValue z

  liftIO $ 0 @=? vx
  liftIO $ 3.1 @=? vy
  liftIO $ -3.1 @=? vz

testFreeVariablesIP :: IO ()
testFreeVariablesIP = assertRunGlpk $ do
  x <- integer
  y <- integer
  z <- integer

  _ <- x @==# 0
  _ <- y @==# 3
  _ <- z @==# -3

  optimizeIP >>= assertFeasible

  vx <- getVariableValue x
  vy <- getVariableValue y
  vz <- getVariableValue z

  liftIO $ 0 @=? vx
  liftIO $ 3 @=? vy
  liftIO $ -3 @=? vz
