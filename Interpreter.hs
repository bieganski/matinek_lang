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
  | VHidden (Value -> Value) -- internal function
   

type Interpret a = ExceptT String (ReaderT Env IO) a


instance Show Value where
  show val = case val of VInt n -> show n
                         VBool b -> show b
                         VClosure s e env -> (show s) ++ " -> " ++ (show e)
                         VADT cname vals -> cname ++ (show vals)
                         VHidden f -> "AAAAAAAAAAAA"

-- data List a = Nil | Cons a (List a)

-- TODO reverseList = foldl (flip (:)) []

--                 Either      [a, b]     Left a    VHidden (\x -> VADT Left [x])
--createConstrFun :: DataName -> [VName] -> Constr -> Value
--createConstrFun dname letters (Constr cname []) = VADT cname
--createConstrFun dname letters (Constr cname (t:ts)) = VHidden (\x -> resval) where
--  resval = createConstrFun dname letters (Constr cname ts)


-- TODO
-- przemyslec to
-- zakodzic
-- dodac monade IO, liftIO
-- obadac typecheck
-- ez 30 pkt

-- "data" declarations changes both variable and
-- type environment (different monads), thus preprocess them first
{-
createEnv :: (ValEnv, DataNameEnv, TypeEnv) -> [Decl] -> (ValEnv, DataNameEnv, TypeEnv)
createEnv (v, d, t) [] = (v, d, t)
createEnv (v, d, t) ((DataDecl dname letters (Constr cname args):constrs)) = createEnv (v', d', t') decls
  where v' = Map.insert cname
createEnv (v, d, t) (_:decls) = createEnv (v, d, t) decls -- omit other than "data" decls
-}
--generateconstr :: String -> [String] -> Constr -> a
--generateconstr dname letters (Constr cname []) = undefined
--generateconstr dname letters (Constr cname (t:ts)) = undefined

localVal :: (ValEnv -> ValEnv) -> Interpret a -> Interpret a
localVal f = local (\(a,b) -> (f a, b))

localData :: (DataNameEnv -> DataNameEnv) -> Interpret a -> Interpret a
localData f = local (\(a, b) -> (a, f b))

askVal :: Interpret ValEnv
askVal = ask >>= return . fst

eval :: Exp -> Interpret Value
eval e = case e of
  EVar s -> do
    env <- askVal
    maybe (throwError ("variable " ++ s ++ "does not exist")) return (Map.lookup s env)
  EApp (ELam x e1) e2 -> eval e2 >>= \res -> localVal (Map.insert x res) (eval e1)
  EApp _ _ -> throwError "thats not applicative!"
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

      
--instance MonadIO Interpret where
--  liftIO res = local id res
      
      
applyClos :: Value -> Value -> Interpret Value
applyClos (VClosure x e env) val = localVal (Map.insert x val) (eval e)
applyClos _ _  = throwError "apply: thats not applicative"

evalOp :: Binop -> Value -> Value -> Value
evalOp Add (VInt v1) (VInt v2) = VInt $ v1 + v2
evalOp Sub (VInt v1) (VInt v2) = VInt $ v1 - v2
evalOp Mul (VInt v1) (VInt v2) = VInt $ v1 * v2
evalOp Eq  (VBool v1) (VBool v2) = VBool $ v1 == v2
evalOp Eq  (VInt v1) (VInt v2) = VBool $ v1 == v2


{-
data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Exp ValEnv
  | VADT ConstrName [Value]
  | VHidden (Value -> Value) -- internal function

data Decl
  = TDecl VName Type
  | DataDecl DataName [VName] [Constr]
  | AssignDecl VName Exp
   deriving (Eq, Ord, Show, Read)
-}
-- type Interpret a = ExceptT String (Reader (ValEnv, DataNameEnv)) a


-- TODO
-- typy: w tej funkcji pojawiają się problemy
 

incArity :: Value -> Value
incArity (VCon arity vadt) = VCon (arity + 1) vadt

addDataConstr :: DataName -> [VName] -> Constr -> ValEnv -> ValEnv
addDataConstr _ _ (Constr cname []) venv = venv
addDataConstr dname letters (Constr cname (t:ts)) venv = if constrExists cname venv
  then error "Interpreter: Constructor name duplication!"
  else addDataConstr dname letters (Constr cname ts) venv' where
    venv' = Map.insert cname cval venv where
      cval = incArity $ Map.findWithDefault (VCon (-1) (VADT cname [])) cname venv


addDataVals :: Decl -> ValEnv -> ValEnv
addDataVals (DataDecl dname letters []) venv = venv
addDataVals (DataDecl dname letters (con:cons)) venv = addDataVals (DataDecl dname letters cons) venv' where
  venv' = addDataConstr dname letters con venv

_cname :: Constr -> ConstrName
_cname (Constr cname _) = cname

dataExists :: DataName -> DataNameEnv -> Bool
dataExists dname denv = elem dname $ Map.elems denv

constrExists :: ConstrName -> ValEnv -> Bool
constrExists cname venv = elem cname $ Map.keys venv

newData :: Decl -> Env -> Env
newData d@(DataDecl dname letters constrs) (venv, denv) = if dataExists dname denv
  then error "Interpreter: Data name duplication!"
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
preprocessDataDecls ( (d@(DataDecl DataName [VName] [Constr]):ds), env ) = preprocessDataDecl (ds, newData d env)
preprocessDataDecls ((d:ds), env) = ((d:ds'), env') where (ds', env') = preprocessDataDecls (ds, env)


evalDecls :: [Decl] -> Interpret Env
evalDecls [] = do
  env <- ask
  return env
evalDecls (d:ds) = case d of TDecl v t -> undefined
                             DataDecl _ _ _ -> throwError "data declarations only at top level supported!"
                             AssignDecl x e -> eval e >>= \ee -> localVal (Map.insert x ee) (evalDecls ds)



-- type Interpret a = ExceptT String (ReaderT (ValEnv, DataNameEnv) IO) a
runInterpreter :: Interpret a -> IO (Either String a)
runInterpreter comp = runReaderT (runExceptT comp) (Map.empty, Map.empty)


interpretMain :: ValEnv -> IO ()
interpretMain venv = case Map.lookup "main" venv of
  Nothing -> putStrLn "Cannot find \"main\" expression!"
  Just val -> putStrLn $ show val
  
main :: IO ()
main = do
  code <- getContents
  let errTree = Par.pProgram $ Par.myLexer code
  case errTree of Err.Bad s -> putStrLn $ "Parser error: " ++ s
                  Err.Ok tree -> do
                    let Program decls = simplify tree
                    res <- runInterpreter (evalDecls decls)
                    case res of Right (venv, _) -> interpretMain venv
                                Left s -> putStrLn $ "Interpreter error: " ++ s


