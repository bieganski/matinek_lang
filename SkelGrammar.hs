module SkelGrammar where

-- Haskell module generated by the BNF converter

import AbsGrammar
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transLowerIdent :: LowerIdent -> Result
transLowerIdent x = case x of
  LowerIdent string -> failure x
transUpperIdent :: UpperIdent -> Result
transUpperIdent x = case x of
  UpperIdent string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Program imports datadecls decls -> failure x
transImport :: Import -> Result
transImport x = case x of
  Import string -> failure x
transType :: Type -> Result
transType x = case x of
  TVar lowerident -> failure x
  TADT upperident types -> failure x
  TArr type_1 type_2 -> failure x
transPat :: Pat -> Result
transPat x = case x of
  PVar lowerident -> failure x
  PCon upperident pats -> failure x
  PLit lit -> failure x
  PAny -> failure x
transBranch :: Branch -> Result
transBranch x = case x of
  Branch pat exp -> failure x
transExp :: Exp -> Result
transExp x = case x of
  EVar lowerident -> failure x
  ECon upperident -> failure x
  ELit lit -> failure x
  EApp exp1 exp2 -> failure x
  EMul exp1 exp2 -> failure x
  EAdd exp1 exp2 -> failure x
  ESub exp1 exp2 -> failure x
  EEq exp1 exp2 -> failure x
  EIf exp1 exp2 exp3 -> failure x
  ELet lowerident exp1 exp2 -> failure x
  ELam lowerident exp -> failure x
  ELst exps -> failure x
  ECase exp branchs -> failure x
transDataDecl :: DataDecl -> Result
transDataDecl x = case x of
  DataDecl upperident loweridents constrs -> failure x
transDecl :: Decl -> Result
transDecl x = case x of
  AssignDecl lowerident exp -> failure x
  FunDecl lowerident1 lowerident2 loweridents exp -> failure x
transConstr :: Constr -> Result
transConstr x = case x of
  Constr upperident types -> failure x
transLit :: Lit -> Result
transLit x = case x of
  LInt integer -> failure x

