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

import Types

import ADTProcessing


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
    evalOp op v1 v2
  ECon cname -> do
    (venv, denv) <- ask
    case Map.lookup cname venv of
      Nothing  -> throwError $ "Constructor " ++ cname ++ " does not exist!"
      Just val -> return val
  ECase e branches -> do
    v <- eval e
    _res <- mapM (uncurry unifyADT) (zip branches (repeat v))
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
  res <- mapM (uncurry unifyADT) (zip branches vals)
  -- all of them should be proper
  if (elem Nothing res)
    then return Nothing
    else do
      env <- foldM saveJoin Map.empty (map fromJust res)
      return $ Just env


unifyADT :: Branch -> Value -> Interpret (Maybe ValEnv)
unifyADT (Branch pat _) v = case pat of
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

evalOp :: Binop -> Value -> Value -> Interpret Value
evalOp Add (VInt v1) (VInt v2) = return $ VInt $ v1 + v2
evalOp Sub (VInt v1) (VInt v2) = return $ VInt $ v1 - v2
evalOp Mul (VInt v1) (VInt v2) = return $ VInt $ v1 * v2
evalOp Eq  (VInt v1) (VInt v2) = return $ VADT res [] where res = if v1 == v2 then "True" else "False"
evalOp op _ _ = throwError $ "Bad arguments for " ++ (show op) ++ " operation"

t0 = TypeEnv Map.empty

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
-- printListSugar v = putStrLn $ show v

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


builtins :: [FilePath]
builtins = [] -- ["./builtins/builtins.hs"]


typeCheck :: NumVar -> TypeEnv -> [Decl] -> Either String TypeEnv
typeCheck _ t [] = Right t
typeCheck ss tenv (d:ds) = case d of
  -- DataDecl dname freeletters constrs -> undefined
  AssignDecl x e -> case doInfer ss e tenv of
    (Left err, _) -> Left (err ++ "\n  at expression " ++ (show x))
    (Right (sub, t), s) -> typeCheck s newtenv ds where newtenv = addScheme x (generalize tenv t) tenv
  _ -> Left "next GHC bug"                                                 


-- throws out data decls
filterDecls :: [Decl] -> [Decl]
filterDecls ((DataDecl _ _ _):ds) = filterDecls ds
filterDecls (d:ds) = d:(filterDecls ds)
filterDecls [] = []

--TODO
                    -- liftIO $ putStrLn $ show $ tree
                    -- postimportenv <- handleImports imports
interpretCode :: String -> Interpret Env
interpretCode code = do
  let errTree = Par.pProgram $ Par.myLexer code
  case errTree of Err.Bad s -> throwError $ s
                  Err.Ok tree -> do
                    let Program imports _decls = simplify tree
                    case runCreateEnv (env0, t0) _decls of
                      Left err -> throwError $ "Static error: " ++ err
                      Right e@((venv, denv), tenv) -> do
                        liftIO $ putStrLn $ show e
                        let decls = filterDecls _decls
                        case typeCheck s0 tenv decls of
                          Left err -> throwError $ "Typecheck error: " ++ err
                          Right tenv' -> liftIO $ putStrLn $ "Ostateczny typeenv: " ++ show tenv'
                        local (const (venv, denv)) (evalDecls decls)
                    

env0 = (Map.empty, Map.empty)

loadBuiltins :: IO Env
loadBuiltins = do
  res <- runInterpreter (handleImports (map Import builtins)) env0
  case res of
    Right env -> return env
    Left s -> error $ "internal error: builitins"



-- TODO : wczytywanie i typowanie builtinsów
main :: IO ()
main = do
  code <- getContents
  envbuiltins <- loadBuiltins
  res <- runInterpreter (interpretCode code) envbuiltins
  case res of
    Right (venv, _) -> printMain venv
    Left s -> putStrLn s
