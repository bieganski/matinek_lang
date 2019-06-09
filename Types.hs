{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Bifunctor (bimap)

import ProgGrammar



{-
Rekonstrukcja typów jest zaimplementowana jako
inferencja Hindley`a-Milner'a, wzorowana na implementacji z linka
http://dev.stephendiehl.com/fun/006_hindley_milner.html,
z inferencją dostosowaną do algebraicznych, polimorficznych data-typów
i wersji wyrażeń z pliku ProgGrammar.hs
-}


intT, boolT :: Type
intT = TADT "Int" []
boolT = TADT "Bool" []

data Scheme = Forall [TVar] Type deriving Show

data TypeEnv = TypeEnv (Map.Map VName Scheme) deriving Show

data Subst = Subst (Map.Map TVar Type) deriving (Eq, Show)

nullSubst :: Subst
nullSubst = Subst Map.empty

-- first applies second one
compose :: Subst -> Subst -> Subst
compose s1@(Subst m1) s2@(Subst m2) = Subst $ Map.map (apply s1) m2 `Map.union` m1


class SubstAble a where
  ftv :: a -> Set.Set TVar
  apply :: Subst -> a -> a

instance SubstAble Type where
  ftv (TVar name) = Set.singleton name
  ftv (TADT _ types) = foldr Set.union Set.empty (map ftv types)
  ftv (TArr t1 t2) = Set.union (ftv t1) (ftv t2)
  apply (Subst s) (TVar name) = Map.findWithDefault (TVar name) name s
  apply s (TADT con lst) = TADT con $ apply s lst
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
unify (TADT a []) (TADT b []) = if a == b
  then return nullSubst
  else throwError $ "cannot unify " ++ (show a) ++ " and " ++ (show b)
unify (TADT a (t:ts)) (TADT b (tt:tts)) = if a /= b
  then throwError $ "cannot unify " ++ (show a) ++ " and " ++ (show b)
  else do
  first <- unify t tt
  rest <- unify (TADT a (apply first ts)) (TADT b (apply first tts))
  return rest
unify t1 t2 = throwError $ (show t1) ++ " |||| " ++ (show t2)

bind :: TVar -> Type -> Infer Subst
bind tv t | occursCheck tv t = throwError $ "cannot unify " ++ (show tv) ++ " and " ++ (show t)
          | TVar tv == t = return nullSubst
          | otherwise = return $ Subst $ Map.singleton tv t



-- instancjonuje znaleziony typ
lookupEnv :: TypeEnv -> VName -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x = do
  case Map.lookup x env of
    Nothing -> throwError $ "cannot find type variable " ++ (show x)
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
  case t1 of TArr _ _ -> do
               ss <- unify (apply s2 t1) (TArr t2 newvar)
               return (ss `compose` s2 `compose` s1, apply ss newvar)
             _ -> throwError $ (show t1) ++ " is not applicable!"
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
infer env (ECon cname) = lookupEnv env cname
infer env (ECase e branches) = do
  (se, te) <- infer env e
  let newenv = apply se env
  (r:rs) <- mapM (flip inferBranch env) branches -- branches not empty (parser)
  foldM foldBranches r rs

foldBranches :: (Subst, Type) -> (Subst, Type) -> Infer (Subst, Type)
foldBranches (s1, t1) (s2, t2) = do
  ss <- unify t1 (apply s1 t2)
  return (compose ss (compose s2 s1), apply ss t2)

inferBranch :: Branch -> TypeEnv -> Infer (Subst, Type)
inferBranch (Branch _ e) env = infer env e


runInfer :: NumVar -> Infer (Subst, Type) -> (Either String (Subst, Type), NumVar)
runInfer s comp = runState (runExceptT comp) s


t1 = TArr (TVar (TV "a")) (TVar (TV "a"))
t2 = TArr intT boolT

s0 = NumVar {num = 0}

--runSubst :: Infer Subst -> Either String Subst
--runSubst comp = evalState (runExceptT comp) s0

doInfer :: NumVar -> Exp -> TypeEnv -> (Either String (Subst, Type), NumVar)
doInfer s e tenv = runInfer s (infer tenv e)
