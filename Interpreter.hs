{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.List

import System.IO

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
   deriving (Eq, Ord)

type Interpret a = ExceptT String (ReaderT Env IO) a


instance Show Value where
  show val = case val of VInt n -> show n
                         VBool b -> show b
                         VClosure x e env -> "\\" ++ (show x) ++ " -> " ++ (show e)
                         VADT cname vals -> "VADT " ++  cname ++ (pprintlst vals)
                         VCon arity val -> "##VCON " ++ (show arity) ++ (show val)


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
  0 -> throwError "internal error -  that should have occured.."
  1 -> return $ VADT cname $ append val vals
  _ -> return $ VCon (n - 1) (VADT cname (append val vals))



-- desugaring
lstToCons :: [Value] -> Value
lstToCons [] = VADT "Nil" []
lstToCons (v:vs) = VADT "Cons" [v, (lstToCons vs)]


eval :: Exp -> Interpret Value
eval e = case e of
  EVar s -> do
    env <- askVal
    maybe (throwError ("variable " ++ s ++ "does not exist")) return (Map.lookup s env)
{-
  EApp (ELam x e1) e2 -> eval e2 >>= \res -> localVal (Map.insert x res) (eval e1)
  EApp c@(ECon cname) e -> do
    cv <- eval c
    case cv of
      VCon 0 _ -> throwError "thats not applicative! (constructor arity)"
      c@(VCon n vadt) -> do
        ev <- eval e
        enhanceVData c ev
-}
  EApp e1 e2 -> do
    lol <- ask
    e1v <- eval e1
    case e1v of VClosure x e venv -> do
                  e2v <- eval e2
                  localVal (const (Map.insert x e2v venv)) (eval e)
                c@(VCon _ _) -> do
                  e2v <- eval e2
                  enhanceVData c e2v
                _ -> throwError $ "thats not applicative!" ++ "\ne1v:" ++ (show e1v) ++ "\nenv:" ++ (show lol)
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
  ELst exps -> do
    vals <- mapM eval exps
    return $ lstToCons vals



applyClos :: Value -> Value -> Interpret Value
applyClos (VClosure x e env) val = localVal (Map.insert x val) (eval e)
applyClos e1 _  = throwError $ (show e1) ++  "is not applicative"

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



-- type Interpret a = ExceptT String (ReaderT (ValEnv, DataNameEnv) IO) a
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
  case errTree of Err.Bad s -> throwError $ "Parser error: " ++ s
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
