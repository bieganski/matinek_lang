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
                      A.TCon (A.UpperIdent s) types -> P.TCon s $ simpl types
                      A.TArr t1 t2 -> P.TArr (simpl t1) (simpl t2)


instance Simplable A.Constr P.Constr where
  simpl :: A.Constr -> P.Constr
  simpl (A.Constr (A.UpperIdent cname) types) = P.Constr cname $ simpl types

instance Simplable A.Decl P.Decl where
  simpl :: A.Decl -> P.Decl
  simpl (A.DataDecl (A.UpperIdent dname) letters constrs) =
    P.DataDecl dname (fetch letters) (simpl constrs)
  simpl (A.AssignDecl (A.LowerIdent var) e) = P.AssignDecl var (simpl e)

instance Simplable A.Import P.Import where
  simpl (A.Import fname) = P.Import fname

instance Simplable A.Program P.Program where
  simpl :: A.Program -> P.Program
  simpl (A.Program imports decls) = P.Program (simpl imports) (simpl decls)

{-
instance Simplable A.Binop P.Binop where
  simpl :: A.Binop -> P.Binop
  simpl op = case op of A.Add -> P.Add
                        A.Sub -> P.Sub
                        A.Mul -> P.Mul
                        A.Eq  -> P.Eq
-}

instance Simplable A.Exp P.Exp where
  simpl :: A.Exp -> P.Exp
  simpl e = case e of
    A.EIf e1 e2 e3 -> P.EIf (simpl e1) (simpl e2) (simpl e3)
    A.ELet id e1 e2 -> P.ELet (fetch id) (simpl e1) (simpl e2)
    A.ELam id e -> P.ELam (fetch id) (simpl e)
    A.EVar id -> P.EVar $ fetch id
    A.ELit (A.LInt n) -> P.ELit (P.LInt n)
    --A.ELit (A.LBool (A.Boolean "True")) -> P.ELit (P.LBool True)
    --A.ELit (A.LBool (A.Boolean "False")) -> P.ELit (P.LBool False)
    --A.ELit (A.LBool (A.Boolean _)) -> error "wtf"
    A.EApp e1 e2 -> P.EApp (simpl e1) (simpl e2)
    A.EAdd e1 e2 -> P.EOp (simpl e1) P.Add (simpl e2)
    A.EMul e1 e2 -> P.EOp (simpl e1) P.Mul (simpl e2)
    A.ECon id -> P.ECon $ fetch id
    A.ELst exps -> P.ELst $ simpl exps
    A.EEq e1 e2 -> P.EOp (simpl e1) P.Eq (simpl e2)
    A.ESub e1 e2 -> P.EOp (simpl e1) P.Sub (simpl e2)
