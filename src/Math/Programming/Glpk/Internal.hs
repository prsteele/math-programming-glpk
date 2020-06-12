{-| This module contains the full definitions backing the simplified API
exposed in 'Math.Programming.Glpk'.
-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Math.Programming.Glpk.Internal where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.IORef
import           Data.List
import           Data.Typeable
import           Data.Void
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

import           Math.Programming
import           Math.Programming.Glpk.Header

-- | An environment to solve math programs using GLPK.
newtype Glpk a = Glpk { _runGlpk :: ExceptT GlpkError (ReaderT GlpkEnv IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader GlpkEnv
    , MonadError GlpkError
    )

instance LPMonad Glpk where
  type Numeric Glpk = Double

  data Variable Glpk
    = Variable { fromVariable :: GlpkVariable }
    deriving
      ( Eq
      , Ord
      , Show
      )

  data Constraint Glpk
    = Constraint { fromConstraint :: GlpkConstraint }
    deriving
      ( Eq
      , Ord
      , Show
      )

  addVariable = addVariable'
  getVariableName = getVariableName'
  setVariableName = setVariableName'
  deleteVariable = deleteVariable'
  addConstraint = addConstraint'
  getConstraintName = getConstraintName'
  setConstraintName = setConstraintName'
  deleteConstraint = deleteConstraint'
  setObjective = setObjective'
  setSense = setSense'
  optimizeLP = optimizeLP'
  setVariableBounds = setVariableBounds'
  getVariableBounds = getVariableBounds'
  getValue = getValue'
  getTimeout = getTimeout'
  setTimeout = setTimeout'
  writeFormulation = writeFormulation'

instance IPMonad Glpk where
  optimizeIP = optimizeIP'
  setVariableDomain = setVariableDomain'
  getVariableDomain = getVariableDomain'
  setRelativeMIPGap = setRelativeMIPGap'
  getRelativeMIPGap = getRelativeMIPGap'

runGlpk :: Glpk a -> IO (Either GlpkError a)
runGlpk glpk = do
  -- Turn off terminal output. If we don't, users won't be able to
  -- inhibit terminal output generated from our setup.
  _ <- glp_term_out glpkOff

  bracket glp_create_prob glp_delete_prob $ \problem -> do
    -- Load the default simplex control parameters
    defaultSimplexControl <- alloca $ \simplexControlPtr -> do
      glp_init_smcp simplexControlPtr
      peek simplexControlPtr

    -- Load the default MIP control parameters
    defaultMipControl <- alloca $ \mipControlPtr -> do
      glp_init_iocp mipControlPtr
      peek mipControlPtr

    -- Turn on presolve, because it seems insane not to.
    --
    -- In particular, this ensures that a naked call to optimizeIP
    -- doesn't fail because of the lack of a basis. Sophisticated users
    -- can control this parameter as they see fit befor the first
    -- optimization call.
    let simplexControl = defaultSimplexControl { smcpPresolve = glpkPresolveOn }
        mipControl = defaultMipControl { iocpPresolve = glpkPresolveOn }

    env <- GlpkEnv problem
           <$> newIORef []
           <*> newIORef []
           <*> newIORef simplexControl
           <*> newIORef mipControl
           <*> newIORef Nothing

    runReaderT (runExceptT (_runGlpk glpk)) env

data SolveType = LP | MIP | InteriorPoint

-- | An interface to the low-level GLPK API.
--
-- High-level solver settings can be modified by altering the
-- 'SimplexMethodControlParameters' and 'MIPControlParameters' values
-- for LP and IP solves, respectively.
data GlpkEnv
  = GlpkEnv
  { _glpkEnvProblem     :: Ptr Problem
  -- ^ A pointer to the Problem object. Most GLPK routines take this
  -- as the first argument.
  , _glpkVariables      :: IORef [GlpkVariable]
  -- ^ The variables in the model
  , _glpkConstraints    :: IORef [GlpkConstraint]
  -- ^ The constraints in the model
  , _glpkSimplexControl :: IORef SimplexMethodControlParameters
  -- ^ The control parameters for the simplex method
  , _glpkMIPControl     :: IORef (MIPControlParameters Void)
  -- ^ The control parameters for the MIP solver
  , _glpkLastSolveType  :: IORef (Maybe SolveType)
  -- ^ The type of the last solve. This is needed to know whether to
  -- retrieve simplex, interior point, or MIP solutions.
  }

data NamedRef a
  = NamedRef
    { namedRefId  :: Int
    , namedRefRef :: IORef a
    }

instance Eq (NamedRef a) where
  x == y = namedRefId x == namedRefId y

instance Ord (NamedRef a) where
  x <= y = namedRefId x <= namedRefId y

instance Show (NamedRef a) where
  show = show . namedRefId

type GlpkConstraint = NamedRef Row

type GlpkVariable = NamedRef Column

askProblem :: Glpk (Ptr Problem)
askProblem = asks _glpkEnvProblem

askVariablesRef :: Glpk (IORef [GlpkVariable])
askVariablesRef = asks _glpkVariables

askConstraintsRef :: Glpk (IORef [GlpkConstraint])
askConstraintsRef = asks _glpkConstraints

register :: Glpk (IORef [NamedRef a]) -> NamedRef a -> Glpk ()
register askRef x = do
  ref <- askRef
  liftIO $ modifyIORef' ref (x :)

unregister :: (Enum a) => Glpk (IORef [NamedRef a]) -> NamedRef a -> Glpk ()
unregister askRef x =
  let
    decrement (NamedRef _ ref) = modifyIORef' ref pred

    mogrify []                  = return ()
    mogrify (z: zs) | z <= x    = return ()
                    | otherwise = decrement z >> mogrify zs
  in do
    ref <- askRef
    liftIO $ do
      -- Remove the element to be unregistered
      modifyIORef' ref (delete x)

      -- Modify the referenced values that were greater than the
      -- referenced element
      readIORef ref >>= mogrify

data GlpkError
  = UnknownVariable GlpkVariable
  | UnknownCode String CInt
  deriving
    ( Show
    )

readColumn :: Variable Glpk -> Glpk Column
readColumn = liftIO . readIORef . namedRefRef . fromVariable

readRow :: Constraint Glpk -> Glpk Row
readRow = liftIO . readIORef . namedRefRef . fromConstraint

addVariable' :: Glpk (Variable Glpk)
addVariable' = do
  problem <- askProblem
  variable <- liftIO $ do
    column <- glp_add_cols problem 1
    columnRef <- newIORef column
    return $ NamedRef (fromIntegral column) columnRef
  register askVariablesRef variable
  return (Variable variable)

setVariableName' :: Variable Glpk -> String -> Glpk ()
setVariableName' variable name = do
    problem <- askProblem
    column <- readColumn variable
    liftIO $ withCString name (glp_set_col_name problem column)

getVariableName' :: Variable Glpk -> Glpk String
getVariableName' variable = do
  problem <- askProblem
  column <- readColumn variable
  liftIO $ glp_get_col_name problem column >>= peekCString

deleteVariable' :: Variable Glpk -> Glpk ()
deleteVariable' variable = do
  problem <- askProblem
  column <- readColumn variable
  liftIO $ allocaGlpkArray [column] (glp_del_cols problem 1)
  unregister askVariablesRef (fromVariable variable)

addConstraint' :: Inequality (LinearExpr Double (Variable Glpk)) -> Glpk (Constraint Glpk)
addConstraint' (Inequality ordering lhs rhs) =
  let
    LinearExpr terms constant = (simplify (lhs .-. rhs)) :: LinearExpr Double (Variable Glpk)

    constraintType :: GlpkConstraintType
    constraintType = case ordering of
      LT -> glpkLT
      GT -> glpkGT
      EQ -> glpkBounded

    constraintRhs :: CDouble
    constraintRhs = realToFrac (negate constant)

    numVars :: CInt
    numVars = fromIntegral (length terms)

    variables :: [Variable Glpk]
    variables = map snd terms

    coefficients :: [CDouble]
    coefficients = map (realToFrac . fst) terms
  in do
    problem <- askProblem
    columns <- mapM readColumn variables
    constraintId <- liftIO $ do
      row <- glp_add_rows problem 1
      rowRef <- newIORef row
      allocaGlpkArray columns $ \columnArr ->
        allocaGlpkArray coefficients $ \coefficientArr -> do
          glp_set_row_bnds problem row constraintType constraintRhs constraintRhs
          glp_set_mat_row problem row numVars columnArr coefficientArr
      return $ NamedRef (fromIntegral row) rowRef

    register askConstraintsRef constraintId
    return (Constraint constraintId)

setConstraintName' :: Constraint Glpk -> String -> Glpk ()
setConstraintName' constraintId name = do
  problem <- askProblem
  row <- readRow constraintId
  liftIO $ withCString name (glp_set_row_name problem row)

getConstraintName' :: Constraint Glpk -> Glpk String
getConstraintName' constraint = do
  problem <- askProblem
  row <- readRow constraint
  liftIO $ glp_get_row_name problem row >>= peekCString

deleteConstraint' :: Constraint Glpk -> Glpk ()
deleteConstraint' constraintId = do
  problem <- askProblem
  row <- readRow constraintId
  liftIO $ allocaGlpkArray [row] (glp_del_rows problem 1)
  unregister askConstraintsRef (fromConstraint constraintId)

setObjective' :: LinearExpr Double (Variable Glpk) -> Glpk ()
setObjective' expr =
  let
    LinearExpr terms constant = simplify expr
  in do
    problem <- askProblem

    -- Set the constant term
    liftIO $ glp_set_obj_coef problem (GlpkInt 0) (realToFrac constant)

    -- Set the variable terms
    forM_ terms $ \(coef, variable) -> do
      column <- readColumn variable
      liftIO $ glp_set_obj_coef problem column (realToFrac coef)

setSense' :: Sense -> Glpk ()
setSense' sense =
  let
    direction = case sense of
      Minimization -> glpkMin
      Maximization -> glpkMax
  in do
    problem <- askProblem
    liftIO $ glp_set_obj_dir problem direction

optimizeLP' :: Glpk SolutionStatus
optimizeLP' =
  let
    convertResult :: Ptr Problem -> GlpkSimplexStatus -> IO SolutionStatus
    convertResult problem result
      | result == glpkSimplexSuccess =
          glp_get_status problem >>= return . solutionStatus
      | otherwise =
          return Error
  in do
    -- Note that we've run an LP solve
    solveTypeRef <- asks _glpkLastSolveType
    liftIO $ writeIORef solveTypeRef (Just LP)

    -- Run Simplex
    problem <- askProblem
    controlRef <- asks _glpkSimplexControl
    liftIO $ do
      control <- readIORef controlRef
      alloca $ \controlPtr -> do
        poke controlPtr control
        result <- glp_simplex problem controlPtr
        convertResult problem result

optimizeIP' :: Glpk SolutionStatus
optimizeIP' =
  let
    convertResult :: Ptr Problem -> GlpkMIPStatus -> IO SolutionStatus
    convertResult problem result
      | result == glpkMIPSuccess =
        glp_mip_status problem >>= return . solutionStatus
      | otherwise =
        return Error
  in do
    -- Note that we've run a MIP solve
    solveTypeRef <- asks _glpkLastSolveType
    liftIO $ writeIORef solveTypeRef (Just MIP)

    problem <- askProblem
    controlRef <- asks _glpkMIPControl
    liftIO $ do
      control <- readIORef controlRef
      alloca $ \controlPtr -> do
        poke controlPtr control
        result <- glp_intopt problem controlPtr
        convertResult problem result

setVariableBounds' :: Variable Glpk -> Bounds Double -> Glpk ()
setVariableBounds' variable bounds =
  let
    (boundType, cLow, cHigh) = case bounds of
      Free              -> (glpkFree, 0, 0)
      NonNegativeReals  -> (glpkGT, 0, 0)
      NonPositiveReals  -> (glpkLT, 0, 0)
      Interval low high -> (glpkBounded, realToFrac low, realToFrac high)
  in do
    problem <- askProblem
    column <- readColumn variable
    liftIO $ glp_set_col_bnds problem column boundType cLow cHigh

getVariableBounds' :: Variable Glpk -> Glpk (Bounds Double)
getVariableBounds' variable =
  let
    boundsFor lb ub | lb == -maxCDouble && ub == maxCDouble = Free
                    | lb == -maxCDouble && ub == 0.0        = NonPositiveReals
                    | lb == 0.0         && ub == maxCDouble = NonNegativeReals
                    | otherwise                             = Interval lb' ub'
      where
        lb' = realToFrac lb
        ub' = realToFrac ub
  in do
    problem <- askProblem
    column <- readColumn variable
    lb <- liftIO (glp_get_col_lb problem column)
    ub <- liftIO (glp_get_col_ub problem column)
    return (boundsFor lb ub)

setVariableDomain' :: Variable Glpk -> Domain -> Glpk ()
setVariableDomain' variable domain =
  let
    vType = case domain of
      Continuous -> glpkContinuous
      Integer    -> glpkInteger
      Binary     -> glpkBinary
  in do
    problem <- askProblem
    column <- readColumn variable
    liftIO $ glp_set_col_kind problem column vType

getVariableDomain' :: Variable Glpk -> Glpk Domain
getVariableDomain' variable =
  let
    getDomain :: GlpkVariableType -> Glpk Domain
    getDomain vType | vType == glpkContinuous = return Continuous
    getDomain vType | vType == glpkInteger    = return Integer
    getDomain vType | vType == glpkBinary     = return Binary
                    | otherwise               = throwError unknownCode
      where
        typeName = show (typeOf vType)
        GlpkVariableType code = vType
        unknownCode = UnknownCode typeName code
  in do
    problem <- askProblem
    column <- readColumn variable
    getDomain =<< liftIO (glp_get_col_kind problem column)

getValue' :: Variable Glpk -> Glpk Double
getValue' variable = do
  lastSolveRef <- asks _glpkLastSolveType
  lastSolve <- (liftIO . readIORef) lastSolveRef

  let method = case lastSolve of
        Nothing            -> glp_get_col_prim
        Just LP            -> glp_get_col_prim
        Just MIP           -> glp_mip_col_val
        Just InteriorPoint -> glp_ipt_col_prim

  problem <- askProblem
  column <- readColumn variable
  liftIO $ realToFrac <$> method problem column

getTimeout' :: RealFrac a => Glpk a
getTimeout' =
  let
    fromMillis :: RealFrac a => CInt -> a
    fromMillis millis = realToFrac millis / 1000
  in do
    controlRef <- asks _glpkSimplexControl
    control <- liftIO (readIORef controlRef)
    return $ fromMillis (smcpTimeLimitMillis control)

setTimeout' :: RealFrac a => a -> Glpk ()
setTimeout' seconds =
  let
    millis :: Integer
    millis = round (seconds * 1000)
  in do
    controlRef <- asks _glpkSimplexControl
    control <- liftIO (readIORef controlRef)
    let control' = control { smcpTimeLimitMillis = fromIntegral millis }
    liftIO (writeIORef controlRef control')

setRelativeMIPGap' :: RealFrac a => a -> Glpk ()
setRelativeMIPGap' gap = do
  controlRef <- asks _glpkMIPControl
  control <- liftIO (readIORef controlRef)
  let control' = control { iocpRelativeMIPGap = realToFrac gap }
  liftIO (writeIORef controlRef control')

getRelativeMIPGap' :: RealFrac a => Glpk a
getRelativeMIPGap' = do
  controlRef <- asks _glpkMIPControl
  control <- liftIO (readIORef controlRef)
  return $ realToFrac (iocpRelativeMIPGap control)

solutionStatus :: GlpkSolutionStatus -> SolutionStatus
solutionStatus status
  | status == glpkOptimal    = Optimal
  | status == glpkFeasible   = Feasible
  | status == glpkInfeasible = Infeasible
  | status == glpkNoFeasible = Infeasible
  | status == glpkUnbounded  = Unbounded
  | otherwise                = Error

writeFormulation' :: FilePath -> Glpk ()
writeFormulation' fileName = do
  problem <- askProblem
  _ <- liftIO $ withCString fileName (glp_write_lp problem nullPtr)
  return ()

maxCDouble :: CDouble
maxCDouble = encodeFloat significand' exponent'
  where
    base = floatRadix (undefined :: CDouble)
    precision = floatDigits (undefined :: CDouble)
    (_, maxExponent) = floatRange (undefined :: CDouble)
    significand' = base ^ precision - 1
    exponent' = maxExponent - precision
