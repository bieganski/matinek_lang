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
type TypesEnv = Map.Map VName Scheme -- istnieje też ADT TypeEnv,
  -- dla wygody tu używamy czystej mapy

type ADTEnv = (ValEnv, DataNameEnv, TypesEnv)

type DeclProcess = ExceptT String (Reader ADTEnv)

-- Either a b -> ForAll [TV 'a', TV 'b'] (TADT "Either" ['a', 'b'])
createADTScheme :: String -> [String] -> Scheme
createADTScheme tname letters = generalize t0 $ TADT tname letters

_cname :: Constr -> ConstrName
_cname (Constr cname _) = cname

dataExists :: DataName -> ADTEnv -> Bool
dataExists dname env = elem dname $ dFun Map.elems env

constrExists :: Constr -> ADTEnv -> Bool
constrExists (Constr cname _) env = elem cname $ tFun Map.keys env


addConstr :: ADTEnv -> Constr -> [String] -> Scheme -> DeclProcess ADTEnv
addConstr e@(venv, denv, tenv) (Constr cname []) letters sch =
  case Map.lookup cname venv of
    Nothing -> Map.insert cname (VADT cname []) venv
    Just _ -> return e -- we are here from recursion (already created)
addConstr (venv, denv, tenv) (Constr cname types) letters sch = do
  
  

parseConstrs :: ADTEnv -> Decl -> Scheme ->DeclProcess ADTEnv
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


newData :: Decl -> ADTEnv ->DeclProcess ADTEnv
newData d@(DataDecl dname letters constrs) (venv, denv, tenv) = do
  when (dataExists dname denv) throwError $ "Data name duplication! (" ++ dname ++ ")"
  let sch = createADTScheme dname letters
  TODO
  

preprocessDataDecls :: ADTEnv -> [Decl] -> DeclProcess ADTEnv
preprocessDataDecls :: env [] -> return env
preprocessDataDecls env d@((DataDecl _ _ _):ds) = do
  newenv <- newData d env
  preprocessDataDecls newenv ds
preprocessDataDecls env _:ds = preprocessDataDecls env ds
