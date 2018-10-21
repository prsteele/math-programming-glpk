module ApiTests where

import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit

import Math.Programming
import Math.Programming.Glpk

test_tree :: TestTree
test_tree = testGroup "API tests"
  [ testCase "Set/get variable names" setGetVariableNameGlpk
  , testCase "Set/get constraint names" setGetConstraintNameGlpk
  ]

setGetVariableName :: (MonadIO m, LPMonad m a) => m ()
setGetVariableName = do
  let name = "foo"
  x <- addVariable `named` name
  vName <- variableName x
  liftIO $ vName @?= name

setGetConstraintName :: (MonadIO m, LPMonad m a) => m ()
setGetConstraintName = do
  let name = "foo"
  x <- addVariable
  c <- addConstraint (1 *: x .>= 0) `named` name
  cName <- constraintName c
  liftIO $ cName @?= name

setGetVariableNameGlpk :: IO ()
setGetVariableNameGlpk = do
  result <- runGlpk setGetVariableName
  case result of
    Left errorMsg -> assertFailure (show errorMsg)
    Right () -> return ()
  return ()

setGetConstraintNameGlpk :: IO ()
setGetConstraintNameGlpk = do
  result <- runGlpk setGetConstraintName
  case result of
    Left errorMsg -> assertFailure (show errorMsg)
    Right () -> return ()
  return ()
