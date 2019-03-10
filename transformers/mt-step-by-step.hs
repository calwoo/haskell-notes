module Transformers where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

-------------------------
-- Types for evaluator --
-------------------------

type VarName = String         -- variable names

data Exp = Lit Integer        -- expressions
            | Var VarName
            | Plus Exp Exp
            | Lambda VarName Exp -- lambda abstraction     
            | Apply Exp Exp      -- function application
            deriving (Show)

data Value = IntVal Integer   -- values
            | FunVal Env VarName Exp
            deriving (Show)

type Env = Map.Map VarName Value -- mapping from names to values

---------------------------
-- Non-monadic evaluator --
---------------------------

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                          in IntVal (i1 + i2)
eval0 env (Lambda var exp) = FunVal env var exp
eval0 env (Apply e1 e2) =
    let fun = eval0 env e1
        val = eval0 env e2
    in case fun of
        FunVal closure var body -> eval0 (Map.insert var val closure) body

-- Test case
exampleExp = Lit 12 `Plus` (Apply (Lambda "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-------------------------
-- Wrapping in a monad --
-------------------------

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return (IntVal i)
eval1 env (Var n) = return $ fromJust (Map.lookup n env)
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Lambda var exp) = return $ FunVal env var exp
eval1 env (Apply e1 e2) =
    do fun <- eval1 env e1
       val <- eval1 env e2
       case fun of
            FunVal closure var body -> eval1 (Map.insert var val closure) body

---------------------------
-- Adding error handling --
---------------------------

type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runExceptT ev)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = return $ fromJust (Map.lookup n env)
eval2a env (Plus e1 e2) = do IntVal i1 <- eval2a env e1
                             IntVal i2 <- eval2a env e2
                             return $ IntVal (i1 + i2)
eval2a env (Lambda var exp) = return $ FunVal env var exp
eval2a env (Apply e1 e2) =
    do fun <- eval2a env e1
       val <- eval2a env e2
       case fun of
            FunVal closure var body -> eval2a (Map.insert var val closure) body 

-- The above evaluator doesn't give error messages. To do so, we change our evaluator slightly.

eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = return $ fromJust (Map.lookup n env)
eval2b env (Plus e1 e2) = do e1' <- eval2b env e1
                             e2' <- eval2b env e2
                             case (e1', e2') of
                                (IntVal i1, IntVal i2) ->
                                    return $ IntVal (i1 + i2)
                                _ -> throwError "type error"
eval2b env (Lambda var exp) = return $ FunVal env var exp
eval2b env (Apply e1 e2) =
    do fun <- eval2b env e1
       val <- eval2b env e2
       case fun of
            FunVal closure var body -> eval2b (Map.insert var val closure) body
            _ -> throwError "type error"

-- A slightly better version is

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
                        Nothing -> throwError ("unbound variable: " ++ n)
                        Just v  -> return v
eval2 env (Plus e1 e2) = do e1' <- eval2 env e1
                            e2' <- eval2 env e2
                            case (e1', e2') of
                                (IntVal i1, IntVal i2) ->
                                    return $ IntVal (i1 + i2)
                                _ -> throwError "type error in addition"
eval2 env (Lambda var exp) = return $ FunVal env var exp
eval2 env (Apply e1 e2) =
    do fun <- eval2 env e1
       val <- eval2 env e2
       case fun of
            FunVal closure var body -> eval2 (Map.insert var val closure) body
            _ -> throwError "type error in application"

----------------------------
-- Hiding the environment --
----------------------------

-- If we want to hide the environment from the function and pass it only when necessary,
-- we can use the reader monad. The reader monad passes a value into a computation and
-- all its sub-computations.

type Eval3 a = ReaderT Env (ExceptT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runExceptT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
                      Nothing -> throwError ("unbound variable: " ++ n)
                      Just v  -> return v
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _ -> throwError "type error in addition"
eval3 (Lambda var exp) = do env <- ask
                            return $ FunVal env var exp
eval3 (Apply e1 e2) = 
    do fun <- eval3 e1
       val <- eval3 e2
       case fun of
            FunVal closure var body -> local (const (Map.insert var val closure)) (eval3 body)
            _ -> throwError "type error in application"

------------------
-- Adding State --
------------------

type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity (runStateT (runExceptT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do
    i <- get
    put (i + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do tick
                   return $ IntVal i
eval4 (Var n) = do tick
                   env <- ask
                   case Map.lookup n env of
                      Nothing -> throwError ("unbound variable: " ++ n)
                      Just v  -> return v
eval4 (Plus e1 e2) = do tick
                        e1' <- eval4 e1
                        e2' <- eval4 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _ -> throwError "type error in addition"
eval4 (Lambda var exp) = do tick
                            env <- ask
                            return $ FunVal env var exp
eval4 (Apply e1 e2) = do tick
                         fun <- eval4 e1
                         val <- eval4 e2
                         case fun of
                            FunVal closure var body -> local (const (Map.insert var val closure)) (eval4 body)
                            _ -> throwError "type error in application"

--------------------
-- Adding logging --
--------------------

type Eval5 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) a

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev = runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do tick
                   return $ IntVal i
eval5 (Var n) = do tick
                   tell [n]
                   env <- ask
                   case Map.lookup n env of
                      Nothing -> throwError ("unbound variable: " ++ n)
                      Just v  -> return v
eval5 (Plus e1 e2) = do tick
                        e1' <- eval5 e1
                        e2' <- eval5 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _ -> throwError "type error in addition"
eval5 (Lambda var exp) = do tick
                            env <- ask
                            return $ FunVal env var exp
eval5 (Apply e1 e2) = do tick
                         fun <- eval5 e1
                         val <- eval5 e2
                         case fun of
                            FunVal closure var body ->
                                local (const (Map.insert var val closure)) (eval5 body)
                            _ -> throwError "type error in application"

--------------------
-- What about IO? --
--------------------

-- The problem is that we can't form an IO monad transformer, as the execution of IO
-- actions in Haskell can't be nested into other monads-- they are only allowed in
-- the IO monad.

-- Fortunately, we just swap out Identity for IO for the base monad of our transforms!

type Eval6 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer IO))) a

runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do tick
                   liftIO $ print i
                   return $ IntVal i
eval6 (Var n) = do tick
                   tell [n]
                   env <- ask
                   case Map.lookup n env of
                      Nothing -> throwError ("unbound variable: " ++ n)
                      Just v  -> return v
eval6 (Plus e1 e2) = do tick
                        e1' <- eval6 e1
                        e2' <- eval6 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _ -> throwError "type error in addition"
eval6 (Lambda var exp) = do tick
                            env <- ask
                            return $ FunVal env var exp
eval6 (Apply e1 e2) = do tick
                         fun <- eval6 e1
                         val <- eval6 e2
                         case fun of
                            FunVal closure var body ->
                                local (const (Map.insert var val closure)) (eval6 body)
                            _ -> throwError "type error in application"