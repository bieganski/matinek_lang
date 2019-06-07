module ADTProcessing where

import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as Map

import Types
import ProgGrammar

type ConstrName = String
type VName = String
type DataName = String

type ValEnv = Map.Map VName Value
type DataNameEnv = Map.Map ConstrName DataName
type TypesEnv = Map.Map VName [Scheme] -- there also exists TypeEnv data,
  -- here we use pure map for convenience

type ADTEnv = (ValEnv, DataNameEnv, TypesEnv)

type DeclProcess = ExceptT String (Reader ADTEnv)

-- Either a b -> ForAll [TV 'a', TV 'b'] (TADT "Either" ['a', 'b'])
createADTScheme :: String -> [String] -> Scheme
createADTScheme dname letters = generalize t0 $ TADT dname letters

dataExists :: DataName -> ADTEnv -> Bool
dataExists dname env = elem dname $ dFun Map.elems env

constrExists :: Constr -> ADTEnv -> Bool
constrExists (Constr cname _) env = elem cname $ tFun Map.keys env


incArity :: Value -> Value
incArity v@(VADT _ []) = (VCon 0 v)
incArity (VADT _ _) = error "must be GHC error to that happen"
incArity (VCon arity vadt) = VCon (arity + 1) vadt


-- parsing constructor we found next type (i.a 'b' in Either a b),
-- we enhance scheme associated with 'Either a'
handleScheme :: TypesEnv -> Type -> [Scheme] -> [Scheme]
handleScheme tenv t schs = (generalize tenv t):schs


addConstrHelp :: ADTEnv -> Constr -> DataName -> [String] -> Scheme -> DeclProcess ADTEnv
addConstrHelp e (Constr cname []) _ _  = e
addConstrHelp e@(venv, denv, tenv) c@(Constr cname (t:ts)) dname letters sch = do
  addConstrHelp (venv', denv, tenv') where
    -- (!) :: Ord k => Map k a -> k -> a
    venv' = Map.insert cname (incArity (venv Map.! cname)) venv -- its like update
    tenv' = Map.insert cname (handleScheme tenv t (tenv Map.! cname)) tenv
    
  
-- does not mess with types, only creates basic stuff
addConstr :: ADTEnv -> Constr -> DataName -> [String] -> Scheme -> DeclProcess ADTEnv
addConstr e@(venv, denv, tenv) c@(Constr cname _) dname letters sch =
  addConstrHelp (venv', denv', tenv') c dname letters where
    venv' = Map.insert cname (VADT cname []) venv
    denv' = Map.insert cname dname
    tenv' = Map.insert cname [sch]
    

parseConstrs :: ADTEnv -> Decl -> Scheme -> DeclProcess ADTEnv
parseConstrs env (DataDecl dname letters []) _ = return env
parseConstrs env (DataDecl dname letters (con:cons)) sch = do
  when (constrExists con env) throwError $ "Constructor name duplication! (" ++ (show con) ++ ")"
  addConstr env con letters sch
  
  

vFun :: (ValEnv -> a) -> ADTEnv -> a
vFun f (v, _, _) = f v

dFun :: (DataEnv -> a) -> ADTEnv -> a
dFun f (_, d, _) = f d

tFun :: (TypesEnv -> a) -> ADTEnv -> a
tFun f (_, _, t) = f t


newData :: Decl -> ADTEnv -> DeclProcess ADTEnv
newData d@(DataDecl dname letters constrs) e@(venv, denv, tenv) = do
  when (dataExists dname denv) throwError $ "Data name duplication! (" ++ dname ++ ")"
  let sch = createADTScheme dname letters
  parseConstrs e d sch
  

-- There are seperate monads for typechecking and program interpreting,
-- but parsing data declarations changes both variable and type environment,
-- thus I decided to preprocess declarations, parse data first and generate
-- enhanced environments, passed to runInterpret
-- return: Non-data declarations (we cannot just omit them in interpreting,
-- for avoiding nested data declarations) and enhanced env.
preprocessDataDecls :: ADTEnv -> [Decl] -> DeclProcess ADTEnv
preprocessDataDecls :: env [] -> return env
preprocessDataDecls env d@((DataDecl _ _ _):ds) = do
  newenv <- newData d env
  preprocessDataDecls newenv ds
preprocessDataDecls env _:ds = preprocessDataDecls env ds
