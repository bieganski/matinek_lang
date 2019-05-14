
module ProgGrammar where


data Program = Program [Import] [Decl]
  deriving (Eq, Ord, Show, Read)


type VName = String
type DataName = String
type ConstrName = String

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
  | EIf Exp Exp Exp
  | EOp Exp Binop Exp
  | ECon ConstrName
  | ELst [Exp]
  | ECase Exp [Branch]
  deriving (Show, Eq, Ord, Read)

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
  | TCon String [Type]
  | TArr Type Type
   deriving (Eq, Ord, Read)

instance Show Type where
  show t = case t of TVar x -> show x
                     TCon s lst -> s ++ (show lst)
                     TArr t1 t2 -> (show t1) ++ " -> " ++ (show t2)

