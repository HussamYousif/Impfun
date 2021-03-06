module Eval where

import Syntax
import Primitive
import Pretty
import Debug.Trace (trace)

import Control.Monad
import Data.IORef


nl = "\n"

addVar :: String -> Value -> Env -> Env
addVar s v env = (s, v):env

addVars :: [String] -> [Value] -> Env -> Env
addVars ss vs env = zip ss vs ++ env

findVar :: String -> Env -> Value
findVar s env =
 let (Just v) = lookup s env in v -- assumes that a variable is always found


exec :: Ast -> IO ()
exec e = steps (e, primitives, [])

steps :: (Ast, Env, [Ctx]) -> IO ()
steps (SSkip, _, []) = return ()
steps st = step st >>= steps

step :: (Ast, Env, [Ctx]) -> IO (Ast, Env, [Ctx])
-- Statement expression: evaluate expression and turn into SSkip
step (SExpr e, env, ctx) = return (e, env, SExpr Hole : ctx)
step (v, env, SExpr Hole : ctx) | isValue v = return (SSkip, env, ctx)

-- Blocks
step (SBlock s, env, ctx) = return (s, env, (SBlock (HoleWithEnv env)) : ctx)
step (SSkip, _, SBlock (HoleWithEnv env) : ctx) = return (SSkip, env, ctx) -- restore environment when block closes

-- Sequences
step (SSeq s1 s2, env, ctx) = return (s1, env, SSeq Hole s2 : ctx)
step (SSkip, env, SSeq Hole s2 : ctx) = return (s2, env, ctx)

-- If and while
step (SIf cond s1 s2, env, ctx) = return (cond, env, SIf Hole s1 s2 : ctx)
step (EVal (VBool True), env, SIf Hole s1 _ : ctx) = return (SBlock s1, env, ctx)
step (EVal (VBool False), env, SIf Hole _ s2 : ctx) = return (SBlock s2, env, ctx)


step (w@(SWhile cond s), env, ctx) = return (SIf cond (SSeq s w) SSkip, env, ctx)

-- Variable declaration
step (SVarDecl s e, env, ctx) = return (e, env, SVarDecl s Hole : ctx)
step (v, env, SVarDecl s Hole : ctx) | isValue v
  = return (SSkip, addVar s (expr2val v) env, ctx)

-- Assignment
step (SAssign s e, env, ctx) = return (e, env, SAssign s Hole : ctx)
step (v, env, SAssign s Hole : ctx) | isValue v = do
  case findVar s env of
    (VRef nv) -> writeIORef nv (expr2val v) >> return (SSkip, env, ctx)
    _ -> ioError $ userError $ "Trying to assign to a non-ref \"" ++ s ++ "\""

-- Variable reference: get from environment
step (EVar s, env, ctx) = return (EVal $ findVar s env, env, ctx)

-- Box a value
step (ERef e, env, ctx) = return (e, env, ERef Hole : ctx)
step (v, env, ERef Hole : ctx) | isValue v = do
  nv <- newIORef (expr2val v)
  return (EVal (VRef nv), env, ctx)


-- Dereference a ref
step (EDeref e, env, ctx) = return (e, env, EDeref Hole : ctx)
step (v, env, EDeref Hole : ctx) | isValue v = do
  let (VRef nv) = expr2val v
  v' <- readIORef nv
  return $ (EVal v', env, ctx)

-- Function becomes a closure
step (EFun pars body, env, ctx) = return (EVal $ VClosure pars body env, env, ctx)

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx) =
  return (s, addVars pars (reverse vs) cloEnv, ECall (HoleWithEnv env) [] vs: ctx)
step (SSkip, _, ECall (HoleWithEnv env) _ _ : ctx) = return (EVal VVoid, env, ctx)
  -- function body fully evaluated, return VVoid

step (ECall (EVal (VPrimFun f)) [] vs, env, ctx) =
  return (EVal $ f (reverse vs), env, ctx) -- The evaluation of primitive values are here.

step (ECall (EVal (VPrimFunIO f)) [] vs, env, ctx) = do
  res  <- f (reverse vs)
  return (EVal res, env, ctx)

step (ECall f [] _, _, _) | isValue f = error $ "a call to non-function " ++ show f

-- Reduce on function position
step (ECall f args [], env, ctx) | notValue f = return (f, env, ECall Hole args [] : ctx) --This shit is important.
step (f, env, ECall Hole args [] : ctx) | isValue f = return (ECall f args [], env, ctx)
step (ECall f (a:args) vs, env, ctx) | isValue f = return (a, env, ECall f (Hole:args) vs : ctx)
step (v, env, ECall f (Hole:args) vs : ctx) | isValue v = return (ECall f args (expr2val v : vs), env, ctx)

-- We need to do the following:
-- Evaluate the expression following the return keyword.
-- stop execution of the function.
-- Return the value associated.
step (SReturn e, env, ctx) | notValue e = return (e, env, SReturn Hole : ctx)
step (e, _, SReturn Hole : ctx) | isValue e =  return (e, env', ctx')
    where (ctx', env')  = contextResolver ctx

{--
The implementation is rather similar to that of the return statment.
Encountering a try/catch statement means leaving a marker in the context, say STry Hole evar s2, where evar is the name of the exception variable and s2 the catch block.
If the try block evaluates normally to SSkip, then the marker is removed and the execution continues after the catch block.
--}
step (STry s1 e s2, env, ctx) = return (s1, env, STry Hole e s2 : ctx)
step (SSkip, env, STry Hole e s2 : ctx) = return (SSkip, env, ctx)

-- First evaluate the throw expression.
step (SThrow e, env, ctx) | notValue e = return (e, env, SThrow Hole : ctx)
step (e, env, SThrow Hole : ctx) | isValue e = return (SThrow e, env, ctx)

step (SThrow e, env, ctx) | isValue e =
                            case enclosedByTry ctx of
                                (Just (STry stmt1 var stmt2), ctx')  -> return (stmt2, addVar var (expr2val e) env , STry stmt1 var Hole : ctx') -- Binding the expression from throw to the catch.
                                (Nothing, [] ) -> error ("Program Termination: " ++ (show .expr2val) e) -- A somewhat dirty solution but hardcoding an impfun printstatement is no better.

step (SSkip, env, STry stmt1 var Hole : ctx) = return (SSkip, env, ctx)

-- When the expression is a value. This will execute the step pattern above this one.
step (e, env, SThrow Hole : ctx) | isValue e = return (SThrow e, env, ctx)



-- For loops start here.
-- A for loop is encountered.
step (SFor init cond inc body, env, ctx) = return (init, env, SFor Hole cond inc body : ctx)

-- Init statement is done evaluating.
-- Skip will mark an executed initstatement. The initStatement will be discarded that way.
step (SSkip, env, SFor Hole cond inc body : ctx) = return (cond , env, SFor SSkip (HoleWithExpr cond) inc body : ctx)

-- The conditional expression is evaluated.
-- Exit the for loop
step (EVal (VBool False), env, SFor _ _ _ _ : ctx) = return (SSkip, env, ctx)

-- unpack the expression and evaluate the increment statement.
step (EVal (VBool True), env, SFor SSkip (HoleWithExpr cond) inc body : ctx) = return (inc, env, SFor SSkip cond (HoleWithStmt inc) body : ctx )

-- The increment is evaluated, evaluate the body.
step (SSkip, env, SFor SSkip cond (HoleWithStmt inc) body : ctx) = return (body, env, SFor SSkip cond inc (HoleWithStmt body) : ctx)

-- The body is evaluated, reEvaluate the condition, restarting the loop.
step (SSkip, env, SFor SSkip cond inc (HoleWithStmt body) : ctx) = return (cond, env, SFor SSkip (HoleWithExpr cond) inc body :ctx)



--step (ast, e, c) | trace ("Show ast: " ++ nl  ++ (show ast) ++ "\n--\n" ) False = undefined

-- Used for return statement to revert the context and reset the environment.
contextResolver :: [Ctx] -> ([Ctx] , Env)
contextResolver [] = error "Did not find the function at the ctx stack"
contextResolver (ECall (HoleWithEnv e) _ _:xs) = (xs, e)
contextResolver (_:xs) = contextResolver xs

enclosedByTry :: [Ctx] -> (Maybe Stmt, [Ctx])
enclosedByTry  [] =  (Nothing , [])
enclosedByTry (STry s1 e s2:xs) = ( Just (STry s1 e s2), xs)
enclosedByTry (_:xs) = enclosedByTry  xs
