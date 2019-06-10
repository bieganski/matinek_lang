
module ProgGrammar where

import Data.Map as Map

type ConstrName = String
type VName = String
type DataName = String

type DataNameEnv = Map.Map ConstrName DataName

type ValEnv = Map.Map VName Value

type Env = (ValEnv, DataNameEnv)



data Program = Program [Import] [Decl]
  deriving (Eq, Ord, Show, Read)


data Decl
  = TDecl VName Type
  | DataDecl DataName [VName] [Constr]
  | AssignDecl VName Exp
   deriving (Eq, Ord, Show, Read)


data Constr = Constr ConstrName [Type]
 deriving (Eq, Ord, Show, Read)


data Exp
  = EVar VName
  | EApp Exp Exp
  | ELam VName Exp
  | ELet VName Exp Exp
  | ELit Lit
  | EOp Exp Binop Exp
  | ECon ConstrName
  | ECase Exp [Branch]
  deriving (Show, Eq, Ord, Read)


data Value
  = VInt Integer
  | VClosure String Exp ValEnv
  | VADT ConstrName [Value]
  | VCon Int Value -- constructor value : arity, VADT
  | VErr String
   deriving (Eq, Ord)

pprintlst :: Show a => [a] -> String
pprintlst [] = " "
pprintlst (x:xs) = " " ++ (show x) ++ (pprintlst xs)


instance Show Value where
  show val = case val of VInt n -> show n
                         VClosure x e env -> "\\" ++ (show x) ++ " -> " ++ (show e)
                         VADT cname vals -> cname ++ (pprintlst vals)
                         VCon arity val -> "(VCON - constr. with arity " ++ (show arity) ++ " - " ++(show val) ++ ")"
                         VErr s -> show s

data Pat
    = PVar VName
    | PCon ConstrName [Pat]
    | PLit Lit
    | PAny
  deriving (Eq, Ord, Show, Read)

data Branch = Branch Pat Exp
  deriving (Eq, Ord, Show, Read)


data Lit
  = LInt Integer
  deriving (Show, Eq, Ord, Read)


data Binop = Add | Sub | Mul | Eq
  deriving (Eq, Ord, Show, Read)

data Import = Import String
  deriving (Eq, Ord, Show, Read)


---- types ----


newtype TVar = TV String
  deriving (Show, Eq, Ord, Read)

data Type
  = TVar TVar
  | TADT String [Type]
  | TArr Type Type
   deriving (Eq, Ord, Read, Show)

{-
instance Show Type where
  show t = case t of TVar x -> show x
                     TADT s lst -> s ++ (show lst)
                     TArr t1 t2 -> (show t1) ++ " -> " ++ (show t2)

-}
