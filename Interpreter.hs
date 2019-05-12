{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified ParGrammar as Par
import qualified ErrM as Err


import ProgGrammar
import Simplify



type ValEnv = Map.Map VName Value
type DataNameEnv = Map.Map ConstrName DataName

type Env = (ValEnv, DataNameEnv)

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Exp ValEnv
  | VADT ConstrName [Value]
  | VCon Int Value -- constructor value : arity, VADT
  | VHidden (Value -> Value) -- internal function # TODO - potrzebne?
   

type Interpret a = ExceptT String (ReaderT Env IO) a


instance Show Value where
  show val = case val of VInt n -> show n
                         VBool b -> show b
                         VClosure s e env -> (show s) ++ " -> " ++ (show e)
                         VADT cname vals -> cname ++ (show vals)
                         VCon arity val -> "##VCON " ++ (show arity) ++ (show val)
                         VHidden f -> "AAAAAAAAAAAA"


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
  0 -> throwError "internal error -  that should hace occured.."
  1 -> return $ VADT cname $ append val vals
  _ -> return $ VCon (n - 1) (VADT cname (append val vals))

eval :: Exp -> Interpret Value
eval e = case e of
  EVar s -> do
    env <- askVal
    maybe (throwError ("variable " ++ s ++ "does not exist")) return (Map.lookup s env)
  EApp (ELam x e1) e2 -> eval e2 >>= \res -> localVal (Map.insert x res) (eval e1)
  EApp c@(ECon cname) e -> do
    cv <- eval c
    case cv of
      VCon 0 _ -> throwError "thats not applicative! (constructor arity)"
      c@(VCon n vadt) -> do
        ev <- eval e
        enhanceVData c ev
  EApp e1 e2 -> throwError $ "thats not applicative!" ++ "\ne1:" ++ (show e1) ++ "\ne2:" ++ (show e2)
  ELam x e -> ask >>= \env -> return $ VClosure x e (fst env)
  ELet x e1 e2 -> eval e1 >>= \res -> localVal (Map.insert x res) (eval e2)
  ELit (LInt n) -> return $ VInt n
  ELit (LBool b) -> return $ VBool b
  EIf cond tr fl -> eval cond >>= \res ->
    case res of VBool True -> eval tr
                VBool False -> eval fl
                _ -> throwError "thats not boolean value!"
  EOp e1 op e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    return $ evalOp op v1 v2
  ECon cname -> do
    (venv, denv) <- ask
    case Map.lookup cname venv of
      Nothing  -> throwError $ "Constructor " ++ cname ++ " does not exist!"
      Just val -> return val



applyClos :: Value -> Value -> Interpret Value
applyClos (VClosure x e env) val = localVal (Map.insert x val) (eval e)
applyClos _ _  = throwError "apply: thats not applicative"

evalOp :: Binop -> Value -> Value -> Value
evalOp Add (VInt v1) (VInt v2) = VInt $ v1 + v2
evalOp Sub (VInt v1) (VInt v2) = VInt $ v1 - v2
evalOp Mul (VInt v1) (VInt v2) = VInt $ v1 * v2
evalOp Eq  (VBool v1) (VBool v2) = VBool $ v1 == v2
evalOp Eq  (VInt v1) (VInt v2) = VBool $ v1 == v2



incArity :: Value -> Value
incArity (VCon arity vadt) = VCon (arity + 1) vadt

-- TODO
-- typy: w tej funkcji pojawiają się problemy
addDataConstr :: DataName -> [VName] -> Constr -> ValEnv -> ValEnv
addDataConstr _ _ (Constr cname []) venv = venv
addDataConstr dname letters (Constr cname (t:ts)) venv = addDataConstr dname letters (Constr cname ts) venv' where
  venv' = Map.insert cname cval venv where
    cval = incArity $ Map.findWithDefault (VCon (-1) (VADT cname [])) cname venv


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
evalDecls (d:ds) = case d of TDecl v t -> undefined
                             DataDecl _ _ _ -> throwError "data declarations only at top level supported!"
                             AssignDecl x e -> eval e >>= \ee -> localVal (Map.insert x ee) (evalDecls ds)





-- type Interpret a = ExceptT String (ReaderT (ValEnv, DataNameEnv) IO) a
runInterpreter :: Interpret a -> Env -> IO (Either String a)
runInterpreter comp env = runReaderT (runExceptT comp) env


interpretMain :: ValEnv -> IO ()
interpretMain venv = case Map.lookup "main" venv of
  Nothing -> putStrLn "Cannot find \"main\" expression!"
  Just val -> putStrLn $ show val

env0 = (Map.empty, Map.empty)

main :: IO ()
main = do
  code <- getContents
  let errTree = Par.pProgram $ Par.myLexer code
  case errTree of Err.Bad s -> putStrLn $ "Parser error: " ++ s
                  Err.Ok tree -> do
                    let Program _decls = simplify tree
                    putStrLn $ show $ tree
                    let (decls, env) = preprocessDataDecls (_decls, env0)
                    res <- runInterpreter (evalDecls decls) env
                    case res of Right (venv, _) -> interpretMain venv
                                Left s -> putStrLn $ "Interpreter error: " ++ s


