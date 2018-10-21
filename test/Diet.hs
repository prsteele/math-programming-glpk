{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Diet where

import Control.Monad
import Control.Monad.Except
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

import Math.Programming
import Math.Programming.Glpk

test_diet :: TestTree
test_diet = testGroup "Diet problem"
  [ testCase "Basic diet problem" glpkBasicDiet
  ]

data Food = Corn | Milk | Bread
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

data Nutrient = Calories | VitaminA
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

basicDiet :: (MonadIO m, LPMonad m Double) => m ()
basicDiet =
  let
    cost :: Food -> Double
    cost Corn = 0.18
    cost Milk = 0.23
    cost Bread = 0.05

    nutrition :: Nutrient -> Food -> Double
    nutrition Calories Corn = 72
    nutrition VitaminA Corn = 107
    nutrition Calories Milk = 121
    nutrition VitaminA Milk = 500
    nutrition Calories Bread = 65
    nutrition VitaminA Bread = 0

    foods :: [Food]
    foods = [Corn, Milk, Bread]

    nutrients :: [Nutrient]
    nutrients = [Calories, VitaminA]

    maxServings :: Double
    maxServings = 10

    nutrientBounds :: Nutrient -> (Double, Double)
    nutrientBounds Calories = (2000, 2250)
    nutrientBounds VitaminA = (5000, 50000)

    expected :: Food -> Double
    expected Corn = 1.94
    expected Milk = 10
    expected Bread = 10

    expectedCost :: Double
    expectedCost = 3.15
  in do
    -- Create the decision variables
    amounts <- forM foods $ \food -> do
      v <- addVariable `within` (Interval 0 maxServings) `named` (printf "amount[%s]" (show food))
      return (food, v)

    -- Create the nutrient constraints
    forM_ nutrients $ \nutrient -> do
      let lhs = sumExpr [nutrition nutrient food *: v | (food, v) <- amounts]
          (lower, upper) = nutrientBounds nutrient
      _ <- (addConstraint $ lhs .<= upper) `named` (printf "%s_max" (show nutrient))
      (addConstraint $ lhs .>= lower) `named` (printf "%s_min" (show nutrient))

    -- Set the objective
    let objective = sumExpr [cost food *: v | (food, v) <- amounts]
    setObjective objective
    setSense Minimization

    -- Solve the problem
    status <- optimizeLP

    -- Check that we reached optimality
    liftIO $ status @?= Optimal
    
    -- Check the variable values
    forM_ amounts $ \(food, v) -> do
      x <- getValue v

      let correct = expected food
          msg = printf
                "Amount of %s should be about %.2f, but is %.3f"
                (show food)
                (realToFrac correct :: Double)
                (realToFrac x :: Double)
      liftIO $ assertBool msg (abs (x - correct) <= 1e-1)

    -- Check the objective value
    objectiveValue <- evalExpr objective
    let msg = printf
              "Objective should be about %.2f, but is %.3f"
              expectedCost
              objectiveValue
    liftIO $ assertBool msg (abs (objectiveValue - expectedCost) < 1e-1)

glpkBasicDiet :: IO ()
glpkBasicDiet = do
  result <- runGlpk basicDiet
  case result of
    Left errorMsg -> assertFailure (show errorMsg)
    Right () -> return ()
  return ()
