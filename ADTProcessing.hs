module ADTProcessing where

import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set

import Types
import ProgGrammar

type ConstrName = String
type VName = String
type DataName = String

type ValEnv = Map.Map VName Value
type DataNameEnv = Map.Map ConstrName DataName
type TypesEnv = Map.Map VName Scheme -- there also exists TypeEnv data,
  -- here we use pure map for convenience

type ADTEnv = (ValEnv, DataNameEnv, TypesEnv)

type DeclProcess = ExceptT String (Reader ADTEnv)

dataExists :: DataName -> ADTEnv -> Bool
dataExists dname env = elem dname $ dFun Map.elems env

constrExists :: Constr -> ADTEnv -> Bool
constrExists (Constr cname _) env = elem cname $ tFun Map.keys env


incArity :: Value -> Value
incArity v@(VADT _ []) = (VCon 0 v)
incArity (VADT _ _) = error "must be GHC error to that happen"
incArity (VCon arity vadt) = VCon (arity + 1) vadt


addConstrHelp :: ADTEnv -> Constr -> DataName -> [String] -> Scheme -> DeclProcess ADTEnv
addConstrHelp e (Constr cname []) _ _  = e
addConstrHelp e@(venv, denv, tenv) c@(Constr cname (t:ts)) dname letters sch = do
  addConstrHelp (venv', denv, tenv') where
    -- (!) :: Ord k => Map k a -> k -> a
    venv' = Map.insert cname (incArity (venv Map.! cname)) venv -- its like update


addConstr :: ADTEnv -> Constr -> DataName -> [String] -> Scheme -> DeclProcess ADTEnv
addConstr e@(venv, denv, tenv) c@(Constr cname types) dname letters sch = do
  resSch = foldr joinTypeScheme sch (reverse types) -- TODO reverse?
  validateJoined resSch
  addConstrHelp (venv', denv', tenv') c dname letters where
    venv' = Map.insert cname (VADT cname []) venv
    denv' = Map.insert cname dname
    tenv' = Map.insert cname resSch
    

parseConstrs :: ADTEnv -> Decl -> Scheme -> DeclProcess ADTEnv
parseConstrs env (DataDecl dname letters []) _ = return env
parseConstrs env (DataDecl dname letters (con:cons)) sch = do
  when (constrExists con env) throwError $ "Constructor name duplication! (" ++ (show con) ++ ")"
  addConstr env con letters sch


joinTypeScheme :: Type -> Scheme -> Scheme
joinTypeScheme t1 (Forall vars t2) = Forall vars (TArrr t1 t2)
-- foldr joinTypeScheme sch types


validateJoined :: Scheme -> DeclProcess ()
validateJoined Forall vars t = do
  let frees = Set.toList $ ftv t
  when (any (flip notElem vars) frees) (throwError "type error: not declared type variable used")
  return ()

vFun :: (ValEnv -> a) -> ADTEnv -> a
vFun f (v, _, _) = f v

dFun :: (DataEnv -> a) -> ADTEnv -> a
dFun f (_, d, _) = f d

tFun :: (TypesEnv -> a) -> ADTEnv -> a
tFun f (_, _, t) = f t

-- Either a b -> ForAll [TV 'a', TV 'b'] (TADT "Either" ['a', 'b'])
createADTScheme :: String -> [String] -> Scheme
createADTScheme dname letters = generalize t0 $ TADT dname letters

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


type Env = (ValEnv, DataNameEnv)

createEnv :: ADTEnv -> [Decl] -> DeclProcess (Env, TypeEnv)
createEnv env, ds = do
  (venv, denv, tenv) <- preprocessDataDecls env ds
  return ((venv, denv), (TypeEnv tenv))


runDeclProcess :: DeclProcess a -> (Env, TypeEnv) -> Either String a
runDeclProcess comp -> ((venv, denv), TypeEnv tenv) = runReader (runExceptT comp) (venv, denv, tenv)


runCreateEnv :: (Env, TypeEnv) -> [Decl] -> Either String (Env, TypeEnv)
runCreateEnv e@((venv, denv), TypeEnv tenv) decls = runDeclProcess (createEnv (venv, denv, tenv) decls) e
