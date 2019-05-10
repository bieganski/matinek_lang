{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified LexGrammar as Lex
import qualified ParGrammar as Par
import qualified SkelGrammar as Skel
import qualified PrintGrammar as Print
import qualified AbsGrammar as Abs
import qualified ErrM as Err


import ProgGrammar
import Simplify

intT, boolT :: Type
intT = TCon "Int" []
boolT = TCon "Bool" []

data Scheme = Forall [TVar] Type

data TypeEnv = TypeEnv (
  Map.Map VName Scheme
  )

data Subst = Subst (Map.Map TVar Type) deriving (Eq, Show)

nullSubst :: Subst
nullSubst = Subst Map.empty

-- najpierw przykłada drugie potem pierwsze
compose :: Subst -> Subst -> Subst
compose s1@(Subst m1) s2@(Subst m2) = Subst $ Map.map (apply s1) m2 `Map.union` m1


class SubstAble a where
  ftv :: a -> Set.Set TVar
  apply :: Subst -> a -> a

instance SubstAble Type where
  ftv (TVar name) = Set.singleton name
  ftv (TCon _ types) = foldr Set.union Set.empty (map ftv types)
  ftv (TArr t1 t2) = Set.union (ftv t1) (ftv t2)
  apply (Subst s) (TVar name) = Map.findWithDefault (TVar name) name s
  apply s (TCon con lst) = TCon con $ apply s lst
  apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)
  
instance SubstAble Scheme where
  ftv (Forall vars t) = Set.difference (ftv t) (Set.fromList vars)
  apply (Subst s) (Forall vars t) = Forall vars t' where
    t' = flip apply t $ Subst $ Map.difference s $ Map.fromList $ zip vars [0..]

instance SubstAble a => SubstAble [a] where
  ftv lst = Set.unions $ map ftv lst
  apply s lst = map (apply s) lst

instance SubstAble TypeEnv where
  ftv (TypeEnv env) = ftv (Map.elems env)
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env


data NumVar = NumVar {num :: Int}

type Infer a = ExceptT String (State NumVar) a 

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s {num = 1 + num s}
  return $ TVar $ TV $ letters !! num s

occursCheck :: SubstAble a => TVar -> a -> Bool
occursCheck tv substable = Set.member tv $ ftv substable

unify ::  Type -> Type -> Infer Subst
unify (TArr x1 y1) (TArr x2 y2) = do
  s1 <- unify x1 x2
  s2 <- unify (apply s1 y1) (apply s1 y2)
  return $ compose s2 s1
unify (TVar tv) t = bind tv t
unify t (TVar tv) = bind tv t
unify (TCon a []) (TCon b []) = return nullSubst
unify (TCon a (t:ts)) (TCon b (tt:tts)) = do
  first <- unify t tt
  rest <- unify (TCon a ts) (TCon b tts)
  if a == b then return (compose first rest) else throwError "cannot unify"

bind :: TVar -> Type -> Infer Subst
bind tv t | occursCheck tv t = throwError "cannot unify"
          | TVar tv == t = return nullSubst
          | otherwise = return $ Subst $ Map.singleton tv t


t1 = TArr (TVar (TV "a")) (TVar (TV "a"))
t2 = TArr intT boolT


s0 = NumVar {num = 0}

runSubst :: Infer Subst -> Either String Subst
runSubst comp = evalState (runExceptT comp) s0

--main :: IO ()
--main = do
--  let lol = runSubst $ unify t1 t2
--  putStrLn $ show lol


-- instancjonuje znaleziony typ
lookupEnv :: TypeEnv -> VName -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x = do
  case Map.lookup x env of
    Nothing -> throwError "cannot find type variable"
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)

instantiate :: Scheme -> Infer Type
instantiate (Forall vars t) = do
  newvars <- mapM (\_ -> fresh) vars
  let subst = Map.fromList $ zip vars newvars
  return $ apply (Subst subst) t


generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall vars t where
  vars = Set.toList $ ftv t `Set.difference` ftv env

addScheme :: VName -> Scheme -> TypeEnv -> TypeEnv
addScheme x sch (TypeEnv env) = TypeEnv $ Map.insert x sch env


opsT :: Map.Map Binop Type
opsT = Map.fromList [
      (Add, (TArr intT (TArr intT intT)))
    , (Mul, (TArr intT (TArr intT intT)))
    , (Sub, (TArr intT (TArr intT intT)))
    , (Eq, (TArr intT (TArr intT boolT)))
  ]


infer :: TypeEnv -> Exp -> Infer (Subst, Type)
infer env (ELit (LInt _)) = return (nullSubst, intT)
infer env (ELit (LBool _)) = return (nullSubst, boolT)
infer env (EVar name) = lookupEnv env name
infer env (ELam x e) = do
  xtvar <- fresh
  let newenv = addScheme x (Forall [] xtvar) env
  (esubst, etype) <- infer newenv e
  return (esubst, TArr (apply esubst xtvar) etype)
infer env (EApp e1 e2) = do
  newvar <- fresh
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (apply s1 env) e2
  ss <- unify (apply s2 t1) (TArr t2 newvar)
  return (compose ss (compose s2 s1), apply ss newvar)
infer env (ELet x e1 e2) = do
  (s1, t1) <- infer env e1
  let xsch = generalize (apply s1 env) t1
  (s2, t2) <- infer (addScheme x xsch env) e2
  return (s1 `compose` s2, t2)
infer env (EOp e1 op e2) = do
  newvar <- fresh
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  ss <- unify (TArr t1 (TArr t2 newvar)) (opsT Map.! op)
  return (s1 `compose` s2 `compose` ss, apply ss newvar)
infer env (EIf cond tt ff) = do
  (s1, t1) <- infer env cond
  (s2, t2) <- infer env tt
  (s3, t3) <- infer env ff
  s4 <- unify t1 boolT
  s5 <- unify t2 t3
  return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, apply s5 t2)

runInfer :: Infer (Subst, Type) -> Either String (Subst, Type)
runInfer comp = evalState (runExceptT comp) s0


--------------------------------------------

type ValEnv = Map.Map VName Value
type DataNameEnv = Map.Map ConstrName DataName

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Exp ValEnv
  | VADT ConstrName [Value]
  | VHidden (Value -> Value) -- internal function
   -- deriving (Eq, Ord, Show)

type Interpret a = ExceptT String (Reader (ValEnv, DataNameEnv)) a


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


main :: IO ()
main = do
  code <- getContents
  let etree = Par.pProgram $ Par.myLexer code
  case etree of Err.Ok tree -> putStrLn $ show $ simplify tree
                Err.Bad s -> putStrLn s


