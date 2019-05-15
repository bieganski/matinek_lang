{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.List

import System.IO

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe(fromJust)

import qualified ParGrammar as Par
import qualified ErrM as Err

import ProgGrammar
import Simplify


type ValEnv = Map.Map VName Value
type DataNameEnv = Map.Map ConstrName DataName

type Env = (ValEnv, DataNameEnv)

data Value
  = VInt Integer
  | VClosure String Exp ValEnv
  | VADT ConstrName [Value]
  | VCon Int Value -- constructor value : arity, VADT
  | VErr String
   deriving (Eq, Ord)

type Interpret a = ExceptT String (ReaderT Env IO) a


instance Show Value where
  show val = case val of VInt n -> show n
                         VClosure x e env -> "\\" ++ (show x) ++ " -> " ++ (show e)
                         VADT cname vals -> "VADT " ++  cname ++ (pprintlst vals)
                         VCon arity val -> "##VCON " ++ (show arity) ++ (show val)
                         VErr s -> show s


pprintlst :: Show a => [a] -> String
pprintlst [] = " "
pprintlst (x:xs) = " " ++ (show x) ++ (pprintlst xs)

localVal :: (ValEnv -> ValEnv) -> Interpret a -> Interpret a
localVal f = local (\(a,b) -> (f a, b))

localData :: (DataNameEnv -> DataNameEnv) -> Interpret a -> Interpret a
localData f = local (\(a, b) -> (a, f b))

askVal :: Interpret ValEnv
askVal = ask >>= return . fst

append :: a -> [a] -> [a]
append x [] = [x]
append x (y:ys) = y : (append x ys)

enhanceVData :: Value -> Value -> Interpret Value
enhanceVData (VCon n (VADT cname vals)) val = case n of
  0 -> throwError "internal error -  that should not have occured.."
  1 -> return $ VADT cname $ append val vals
  _ -> return $ VCon (n - 1) (VADT cname (append val vals))



-- desugaring
lstToCons :: [Value] -> Value
lstToCons [] = VADT "Nil" []
lstToCons (v:vs) = VADT "Cons" [v, (lstToCons vs)]



tt, ff :: Value
tt = VADT "True" []
ff = VADT "False" []


eval :: Exp -> Interpret Value
eval e = case e of
  EVar s -> do
    env <- askVal
    maybe (throwError ("variable " ++ (show s) ++ " does not exist")) return (Map.lookup s env)
  EApp e1 e2 -> do
    lol <- ask
    e1v <- eval e1
    case e1v of VClosure x e venv -> do
                  e2v <- eval e2
                  localVal (const (Map.insert x e2v venv)) (eval e)
                c@(VCon _ _) -> do
                  e2v <- eval e2
                  enhanceVData c e2v
                _ -> throwError $ "thats not applicative!" ++ "\nexp:" ++ (show e1v)
  ELam x e -> ask >>= \env -> return $ VClosure x e (fst env)
  ELet x e1 e2 -> do
    (env, _) <- ask
    rec newenv <- localVal (const newenv) $ (eval e1) >>= \d -> return (Map.insert x d env)
    localVal (const newenv) $ eval e2
  ELit (LInt n) -> return $ VInt n
  EOp e1 op e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    return $ evalOp op v1 v2
  ECon cname -> do
    (venv, denv) <- ask
    case Map.lookup cname venv of
      Nothing  -> throwError $ "Constructor " ++ cname ++ " does not exist!"
      Just val -> return val
  ECase e branches -> do
    v <- eval e
    _res <- mapM (uncurry unify) (zip branches (repeat v))
    let res = zip _res branches
    case filter ((/=Nothing) . fst) res of
      [] -> throwError "Couldn't match any pattern!"
      [match] -> evalMatch match
      (match:_) -> do
        liftIO $ putStrLn $ "warning: more than one matching, executing first one."
        evalMatch match


evalMatch :: (Maybe ValEnv, Branch) -> Interpret Value
evalMatch (Nothing, _) = throwError "evalMatch internal error"
evalMatch (Just venv, Branch _ e) = localVal (Map.union venv) (eval e)


saveJoin :: ValEnv -> ValEnv -> Interpret ValEnv
saveJoin m1 m2 = do
  let overlaps = Map.keys (Map.intersection m1 m2)
  if  overlaps /= []
    then throwError $ "overlapping keys: " ++ (show overlaps)
    else return $ Map.union m1 m2


unifyConstr :: [Pat] -> [Value] -> Interpret (Maybe ValEnv)
unifyConstr [] [] = return $ Just Map.empty
unifyConstr [] _ = throwError $ "TODO czesciowa aplikacja"
unifyConstr pats vals = if length pats /= length vals then return Nothing else do
  let branches = map (flip Branch (EVar "2137")) pats
  res <- mapM (uncurry unify) (zip branches vals)
  -- all of them should be proper
  if (elem Nothing res)
    then return Nothing
    else do
      env <- foldM saveJoin Map.empty (map fromJust res)
      return $ Just env


unify :: Branch -> Value -> Interpret (Maybe ValEnv)
unify (Branch pat _) v = case pat of
  PVar x -> return $ Just $ Map.singleton x v
  PCon cname pats -> do
    venv <- askVal
    case v of
      VADT cname' vals -> if cname == cname' then unifyConstr pats vals else return Nothing
      VCon n (VADT cname' []) -> if cname /= cname' then return Nothing else do
        liftIO (putStrLn "warning: partial application")
        return Nothing -- TODO
  PLit (LInt int) -> case v of
    VInt int' -> if int == int' then return $ Just Map.empty else return Nothing
    _ -> throwError "Cannot unify literal with nonliteral!"
  PAny -> return $ Just Map.empty


applyClos :: Value -> Value -> Interpret Value
applyClos (VClosure x e env) val = localVal (Map.insert x val) (eval e)
applyClos e1 _  = throwError $ (show e1) ++  "is not applicative"

evalOp :: Binop -> Value -> Value -> Value
evalOp Add (VInt v1) (VInt v2) = VInt $ v1 + v2
evalOp Sub (VInt v1) (VInt v2) = VInt $ v1 - v2
evalOp Mul (VInt v1) (VInt v2) = VInt $ v1 * v2
evalOp Eq  (VInt v1) (VInt v2) = VADT res [] where res = if v1 == v2 then "True" else "False"



incArity :: Value -> Value
incArity (VCon arity vadt) = VCon (arity + 1) vadt

addDataConstr :: DataName -> [VName] -> Constr -> ValEnv -> ValEnv
addDataConstr _ _ (Constr cname []) venv = case Map.lookup cname venv of
  Nothing -> Map.insert cname (VADT cname []) venv
  Just _ -> venv
addDataConstr dname letters (Constr cname (t:ts)) venv = addDataConstr dname letters (Constr cname ts) venv' where
  venv' = Map.insert cname cval venv where
    cval = incArity $ Map.findWithDefault (VCon 0 (VADT cname [])) cname venv


addDataVals :: Decl -> ValEnv -> ValEnv
addDataVals (DataDecl dname letters []) venv = venv
addDataVals (DataDecl dname letters (con:cons)) venv = if constrExists con venv
  then error $ "Constructor name (" ++ (show con) ++ ") duplication!"
  else addDataVals (DataDecl dname letters cons) venv' where
    venv' = addDataConstr dname letters con venv

_cname :: Constr -> ConstrName
_cname (Constr cname _) = cname

dataExists :: DataName -> DataNameEnv -> Bool
dataExists dname denv = elem dname $ Map.elems denv

constrExists :: Constr -> ValEnv -> Bool
constrExists (Constr cname _) venv = elem cname $ Map.keys venv

newData :: Decl -> Env -> Env
newData d@(DataDecl dname letters constrs) (venv, denv) = if dataExists dname denv
  then error "Data name duplication!"
  else (venv', denv') where
    denv' = Map.union denv $ Map.fromList $ zip (map _cname constrs) (repeat dname)
    venv' = addDataVals d venv

-- There are seperate monads for typechecking and program interpreting,
-- but parsing data declarations changes both variable and type environment,
-- thus I decided to preprocess declarations, parse data first and generate
-- enhanced environments, passed to runInterpret
-- return: Non-data declarations (we cannot just omit them in interpreting,
-- for avoiding nested data declarations) and enhanced env.
preprocessDataDecls :: ([Decl], Env) -> ([Decl], Env)
preprocessDataDecls ([], env) = ([], env)
preprocessDataDecls ( (d@(DataDecl _ _ _):ds), env ) = preprocessDataDecls (ds, newData d env)
preprocessDataDecls ((d:ds), env) = ((d:ds'), env') where (ds', env') = preprocessDataDecls (ds, env)


evalDecls :: [Decl] -> Interpret Env
evalDecls [] = do
  env <- ask
  return env
evalDecls (d:ds) = case d
  of TDecl v t -> undefined
     DataDecl _ _ _ -> throwError "data declarations only at top level supported!"
     AssignDecl x e -> do
       venv <- askVal
       when (elem x (Map.keys venv)) $ liftIO $ putStrLn $ "warning: overwriting " ++ (show x) ++ " variable."
       eval e >>= \ee -> localVal (Map.insert x ee) (evalDecls ds)


runInterpreter :: Interpret a -> Env -> IO (Either String a)
runInterpreter comp env = runReaderT (runExceptT comp) env


printListSugarNonEmpty :: Value -> Bool -> IO ()
printListSugarNonEmpty (VADT "Nil" []) False = putStrLn "]"
printListSugarNonEmpty (VADT "Cons" [v, vs]) first = do
  when first $ putStr "["
  when (not first) $ putStr " "
  putStr $ (show v) ++ ","
  printListSugarNonEmpty vs False

printListSugar :: Value -> IO ()
printListSugar (VADT "Nil" []) = putStrLn "[]"
printListSugar lst@(VADT "Cons" [v, vs]) = printListSugarNonEmpty lst True

printMain :: ValEnv -> IO ()
printMain venv = case Map.lookup "main" venv of
  Nothing -> putStrLn "Cannot find \"main\" expression!"
  Just val -> case val of lst@(VADT "Cons" _) -> printListSugar val
                          _ -> putStrLn $ show val



loadModule :: Import -> IO String
loadModule (Import filename) = do
  h <- openFile filename ReadMode
  code <- hGetContents h
  return code


_handleImport :: Import -> Interpret Env
_handleImport im = do
  code <- liftIO $ loadModule im
  interpretCode code


handleImports :: [Import] -> Interpret Env
handleImports [] = ask
handleImports (x:xs) = do
  env <- _handleImport x
  local (const env) (handleImports xs)


_builtins :: [FilePath]
_builtins = ["./builtins/builtins.hs"]

interpretCode :: String -> Interpret Env
interpretCode code = do
  let errTree = Par.pProgram $ Par.myLexer code
  case errTree of Err.Bad s -> throwError $ s
                  Err.Ok tree -> do
                    let Program imports _decls = simplify tree
                    -- putStrLn $ show $ tree
                    postimportenv <- handleImports imports
                    let (decls, env) = preprocessDataDecls (_decls, postimportenv)
                    local (const env) (evalDecls decls)
                    

env0 = (Map.empty, Map.empty)

loadBuiltins :: IO Env
loadBuiltins = do
  res <- runInterpreter (handleImports (map Import _builtins)) env0
  case res of
    Right env -> return env
    Left s -> error $ "internal error: builitins"


main :: IO ()
main = do
  code <- getContents
  envbuiltins <- loadBuiltins
  res <- runInterpreter (interpretCode code) envbuiltins
  case res of
    Right (venv, _) -> printMain venv
    Left s -> putStrLn $ "Interpreter error: " ++ s
