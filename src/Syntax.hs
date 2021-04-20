module Syntax where

import Data.IORef

data Ast =
    SSkip
  | SIf Expr Stmt Stmt
  | SWhile Expr Stmt
  | SBlock Stmt
  | SSeq Stmt Stmt
  | SAssign String Expr
  | SVarDecl String Expr
  | SExpr Expr
  | SReturn Expr
  | STry Stmt String Stmt -- Try {stmt1} catch(identifier) {stmt2}
  | SThrow Expr
  | SFor Stmt Expr Stmt Stmt -- for(stmt;expr;stmt) {stmt}

  | EVal Value
  | EVar String
  | EFun [String] Stmt
  | ECall Expr [Expr] [Value]
  | ERef Expr
  | EDeref Expr

  | Hole
  | HoleWithEnv Env

  -- For evaluation of a phrase, where the unevaluated phrase needs to be stored.
  | HoleWithExpr Expr
  | HoleWithStmt Stmt

  -- My typing here.
  -- | UserDefinedType String [Stmt]
    deriving Show

type Stmt = Ast
type Expr = Ast
type Ctx = Ast

data Value =
    VList [Value]
  | VInt Int
  | VBool Bool
  | VString String
  | VRef (IORef Value)
  | VVoid
  | VClosure [String] Stmt Env
  | VPrimFun ([Value] -> Value)
  | VPrimFunIO ([Value] -> IO Value)


isValue, notValue :: Ast -> Bool
isValue (EVal _) = True
isValue _ = False
notValue = not . isValue

expr2val :: Expr -> Value
expr2val (EVal v) = v

type Env = [(String, Value)]

instance Show Value where
  show (VList a) = show a
  show (VInt i) = show i
  show (VBool b) = show b
  show (VString s) = s
  show (VRef _) = "ref"
  show (VVoid) = "void"
  show (VClosure _ _ _) = "closure"
  show (VPrimFun _) = "prim-fun"
  show (VPrimFunIO _) = "prim-fun io"
