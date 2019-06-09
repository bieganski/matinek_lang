module ADTProcessing where

import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set

import Types
import ProgGrammar

-- there also exists TypeEnv data,
-- here we use pure map for convenience
type TypesEnv = Map.Map VName Scheme 
type ADTEnv = (ValEnv, DataNameEnv, TypesEnv)

type DeclProcess = ExceptT String (Reader ADTEnv)

dataExists :: DataName -> ADTEnv -> Bool
dataExists dname env = elem dname $ dFun Map.elems env

constrExists :: Constr -> ADTEnv -> Bool
constrExists (Constr cname _) env = elem cname $ tFun Map.keys env


incArity :: Value -> Value
incArity v@(VADT _ []) = (VCon 1 v)
incArity (VADT _ _) = error "must be GHC error to that happen"
incArity (VCon arity vadt) = VCon (arity + 1) vadt


addConstrHelp :: ADTEnv -> Constr -> DataName -> DeclProcess ADTEnv
addConstrHelp e (Constr cname []) _ = return e
addConstrHelp e@(venv, denv, tenv) (Constr cname (t:ts)) dname =
  addConstrHelp (venv', denv, tenv) (Constr cname ts) dname where
    -- (!) :: Ord k => Map k a -> k -> a
    venv' = Map.insert cname (incArity (venv Map.! cname)) venv -- its like update


addConstr :: ADTEnv -> Constr -> DataName  -> Scheme -> DeclProcess ADTEnv
addConstr e@(venv, denv, tenv) c@(Constr cname types) dname sch = do
  validateJoined (foldr joinTypeScheme sch (reverse types))
  addConstrHelp (venv', denv', tenv') c dname where
    venv' = Map.insert cname (VADT cname []) venv
    denv' = Map.insert cname dname denv
    tenv' = Map.insert cname (foldr joinTypeScheme sch (reverse types)) tenv
    

parseConstrs :: ADTEnv -> Decl -> Scheme -> DeclProcess ADTEnv
parseConstrs env (DataDecl dname _ []) _ = return env
parseConstrs env (DataDecl dname letters (con:cons)) sch = do
  if constrExists con env
    then throwError $ "Constructor name duplication! (" ++ (show con) ++ ")"
    else do
    env' <- addConstr env con dname sch
    local (const env') parseConstrs env' (DataDecl dname letters cons) sch


joinTypeScheme :: Type -> Scheme -> Scheme
joinTypeScheme t1 (Forall vars t2) = Forall vars (TArr t1 t2)
-- foldr joinTypeScheme sch types


validateJoined :: Scheme -> DeclProcess ()
validateJoined (Forall vars t) = do
  let frees = Set.toList $ ftv t
  when (any (flip notElem vars) frees) (throwError "type error: not declared type variable used")
  return ()

vFun :: (ValEnv -> a) -> ADTEnv -> a
vFun f (v, _, _) = f v

dFun :: (DataNameEnv -> a) -> ADTEnv -> a
dFun f (_, d, _) = f d

tFun :: (TypesEnv -> a) -> ADTEnv -> a
tFun f (_, _, t) = f t

-- Either a b -> ForAll [TV 'a', TV 'b'] (TADT "Either" ['a', 'b'])
-- TODO sprawdzic unikalnosc, Either a a
createADTScheme :: String -> [String] -> Scheme
createADTScheme dname letters = Forall (map TV letters) $ TADT dname $ map (TVar . TV) letters

newData :: Decl -> ADTEnv -> DeclProcess ADTEnv
newData d@(DataDecl dname letters constrs) e = do
  if dataExists dname e
    then throwError $ "Data name duplication! (" ++ dname ++ ")"
    else parseConstrs e d sch where sch = createADTScheme dname letters
  
  
-- There are seperate monads for typechecking and program interpreting,
-- but parsing data declarations changes both variable and type environment,
-- thus I decided to preprocess declarations, parse data first and generate
-- enhanced environments, passed to runInterpret
-- return: Non-data declarations (we cannot just omit them in interpreting,
-- for avoiding nested data declarations) and enhanced env.
preprocessDataDecls :: ADTEnv -> [Decl] -> DeclProcess ADTEnv
preprocessDataDecls env [] = return env
preprocessDataDecls env (d@(DataDecl _ _ _):ds) = do
  newenv <- newData d env
  preprocessDataDecls newenv ds
preprocessDataDecls env (_:ds) = preprocessDataDecls env ds


createEnv :: ADTEnv -> [Decl] -> DeclProcess (Env, TypeEnv)
createEnv env ds = do
  (venv, denv, tenv) <- preprocessDataDecls env ds
  return ((venv, denv), (TypeEnv tenv))


runDeclProcess :: DeclProcess a -> (Env, TypeEnv) -> Either String a
runDeclProcess comp ((venv, denv), TypeEnv tenv) = runReader (runExceptT comp) (venv, denv, tenv)


runCreateEnv :: (Env, TypeEnv) -> [Decl] -> Either String (Env, TypeEnv)
runCreateEnv e@((venv, denv), TypeEnv tenv) decls = runDeclProcess (createEnv (venv, denv, tenv) decls) e
