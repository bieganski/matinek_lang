{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Simplify where

import qualified AbsGrammar as A
import qualified ProgGrammar as P


simplify :: A.Program -> P.Program
simplify p = simpl p


class Fetchable a b where
  fetch :: a -> b

instance Fetchable A.LowerIdent String where
  fetch (A.LowerIdent id) = id

instance Fetchable A.UpperIdent String where
  fetch (A.UpperIdent id) = id

instance Fetchable a b => Fetchable [a] [b] where
  fetch lst = map fetch lst



class Simplable a b where
  simpl :: a -> b

instance Simplable a b => Simplable [a] [b] where
  simpl lst = map simpl lst

instance Simplable A.Type P.Type where
  simpl :: A.Type -> P.Type
  simpl t = case t of A.TVar (A.LowerIdent s) -> P.TVar $ P.TV s
                      A.TADT (A.UpperIdent s) types -> P.TADT s $ simpl types
                      A.TArr t1 t2 -> P.TArr (simpl t1) (simpl t2)
                      

instance Simplable A.Constr P.Constr where
  simpl :: A.Constr -> P.Constr
  simpl (A.Constr (A.UpperIdent cname) types) = P.Constr cname $ simpl types

instance Simplable A.Decl P.Decl where
  simpl :: A.Decl -> P.Decl
  
  simpl (A.AssignDecl (A.LowerIdent var) e) = P.AssignDecl var (simpl e)
  -- simpl (A.FunDecl f x1 xs e) = simpl $ A.AssignDecl f (foldr A.ELam e (x1:xs))
  -- for recursion purposes I did it another way
  simpl (A.FunDecl f x1 xs e) = simpl $ A.AssignDecl f $ A.ELet f (foldr A.ELam e (x1:xs)) (A.EVar f)

instance Simplable A.DataDecl P.Decl where
  simpl (A.DataDecl (A.UpperIdent dname) letters constrs) =
    P.DataDecl dname (fetch letters) (simpl constrs)
    
instance Simplable A.Import P.Import where
  simpl (A.Import fname) = P.Import fname

instance Simplable A.Program P.Program where
  simpl :: A.Program -> P.Program
  simpl (A.Program imports ddecls decls) =
    P.Program (simpl imports) ((simpl ddecls) ++ (simpl decls))


simplCons :: [P.Exp] -> P.Exp
simplCons [] = P.ECon "Nil"
simplCons (e:es) = P.EApp (P.EApp (P.ECon "Cons") e) (simplCons es)


simplIf :: A.Exp -> P.Exp
simplIf (A.EIf e1 e2 e3) = P.ECase (simpl e1) [b1, b2] where
  b1 = simpl $ A.Branch (A.PCon (A.UpperIdent "True") []) e2
  b2 = simpl $ A.Branch (A.PCon (A.UpperIdent "False") []) e3

instance Simplable A.Exp P.Exp where
  simpl :: A.Exp -> P.Exp
  simpl e = case e of
    A.EIf e1 e2 e3 -> simplIf (A.EIf e1 e2 e3)
    A.ELet id e1 e2 -> P.ELet (fetch id) (simpl e1) (simpl e2)
    A.ELam id e -> P.ELam (fetch id) (simpl e)
    A.EVar id -> P.EVar $ fetch id
    A.ELit (A.LInt n) -> P.ELit (P.LInt n)
    A.EApp e1 e2 -> P.EApp (simpl e1) (simpl e2)
    A.EAdd e1 e2 -> P.EOp (simpl e1) P.Add (simpl e2)
    A.EMul e1 e2 -> P.EOp (simpl e1) P.Mul (simpl e2)
    A.ECon id -> P.ECon $ fetch id
    A.ELst exps -> simplCons $ simpl exps
    A.EEq e1 e2 -> P.EOp (simpl e1) P.Eq (simpl e2)
    A.ESub e1 e2 -> P.EOp (simpl e1) P.Sub (simpl e2)
    A.ECase e branches -> P.ECase (simpl e) (simpl branches)


instance Simplable A.Lit P.Lit where
  simpl (A.LInt int) = P.LInt int


instance Simplable A.Pat P.Pat where
  simpl p = case p of
    A.PVar id ->P.PVar $ fetch id
    A.PCon id pats -> P.PCon (fetch id) (simpl pats)
    A.PLit lit -> P.PLit (simpl lit)
    A.PAny -> P.PAny

instance Simplable A.Branch P.Branch where
  simpl (A.Branch pat exp) = P.Branch (simpl pat) (simpl exp)
