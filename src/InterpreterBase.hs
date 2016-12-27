-- I want these language extensions for my syntactic sugaring tricks at the end

 {-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-} 

 module InterpreterBase where

-- I want my own definition of lookup and I want to write my own function
-- named "print".

 import Prelude hiding (lookup, print)

 import qualified Data.Map as Map
 import Data.Maybe

-- I want to get at the standard "print" function using the name System.print

 import qualified System.IO as System

-- I plan to use these monads to construct the parts of my interpreter

 import Control.Monad.Identity
 import Control.Monad.Except
 import Control.Monad.Reader
 import Control.Monad.State
 import Control.Monad.Writer

-- {-------------------------------------------------------------------}
-- {- The pure expression language                                    -}
-- {-------------------------------------------------------------------}

 data Val = I Int | B Bool
            deriving (Eq, Show, Read)

 data Expr = Const Val
      | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
      | And Expr Expr | Or Expr Expr | Not Expr 
      | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
      | Var String
    deriving (Eq, Show, Read)

 type Name = String 
 type Env = Map.Map Name Val

 lookup k t = case Map.lookup k t of
                Just x -> return x
                Nothing -> fail ("Unknown variable "++k)

-- {-- Monadic style expression evaluator, 
--  -- with error handling and Reader monad instance to carry dictionary
--  --}

 type Eval a = ReaderT Env (ExceptT String Identity) a 
 runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

-- This evaluator could be a little neater 

-- Integer typed expressions

 evali op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ I (i0 `op` i1)
                          _            -> fail "type error in arithmetic expression"

-- Boolean typed expressions

 evalb op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (B i0, B i1) -> return $ B (i0 `op` i1)
                          _            -> fail "type error in boolean expression"

-- Operations over integers which produce booleans

 evalib op e0 e1 = do e0' <- eval e0
                      e1' <- eval e1
                      case (e0', e1') of
                           (I i0, I i1) -> return $ B (i0 `op` i1)
                           _            -> fail "type error in arithmetic expression"

-- Evaluate an expression

 eval :: Expr -> Eval Val
 eval (Const v) = return v
 eval (Add e0 e1) = do evali (+) e0 e1
 eval (Sub e0 e1) = do evali (-) e0 e1
 eval (Mul e0 e1) = do evali (*) e0 e1
 eval (Div e0 e1) = do evali div e0 e1

 eval (And e0 e1) = do evalb (&&) e0 e1
 eval (Or e0 e1) = do evalb (||) e0 e1

 eval (Not e0  ) = do evalb (const not) e0 (Const (B True)) 
                        where not2 a _ = not a -- hack, hack

 eval (Eq e0 e1) = do evalib (==) e0 e1
 eval (Gt e0 e1) = do evalib (>) e0 e1
 eval (Lt e0 e1) = do evalib (<) e0 e1

 eval (Var s) = do env <- ask
                   lookup s env


-- {-------------------------------------------------------------------}
-- {- The statement language                                          -}


 data Statement = Assign String Expr
                | If Expr Statement Statement
                | While Expr Statement
                | Print Expr
                | Seq Statement Statement
                | Try Statement Statement
                | Pass                    
       deriving (Eq, Show, Read)

-- make Statement an instance of monad
 instance Monoid Statement where
  mempty = Pass
  mappend a b = a `Seq` b


-- {-- Monadic style statement evaluator, 
--  -- with error handling and Reader monad instance to carry dictionary
--  --}
  
 -- allows a list of executed statements to be passed around. This should be useful for rewinding 
 type BigEnv = ([Statement], Env)


 type Run a = StateT BigEnv (ExceptT String IO) a
 runRun p = runExceptT ( runStateT p ([], Map.empty))

 set :: (Name, Val) -> Run ()
 set (s, i) = state $ (\(list, table) -> ((), (list, Map.insert s i table)))

 exec :: Statement -> Run ()
 exec (Assign s v) = do (_, st) <- get
                        Right val <- return $ runEval st (eval v)
                        set (s, val)

 exec (Seq s0 s1) = do execCheck s0 >> execCheck s1

 exec (Print e) = do  (_, st) <- get
                      Right val <- return $ runEval st (eval e)
                      liftIO $ System.print val
                      return ()

 exec (If cond s0 s1) = do  (_, st) <- get
                            Right (B val) <- return $ runEval st (eval cond)
                            if val then do exec s0 else do exec s1

 exec (While cond s) = do (_, st) <- get
                          Right (B val) <- return $ runEval st (eval cond)
                          if val then do exec s >> exec (While cond s) else return ()

 exec (Try s0 s1) = do catchError (exec s0) (\e -> exec s1)

 exec Pass = return ()

-- {-- Syntactic Sugar --}
 int = Const . I
 bool = Const . B
 var = Var

 
-- logic for asking the user whether or not to execute a section of code
 execCheck :: Statement -> Run ()
 execCheck s = do liftIO $ System.print $ "Execute '"++(show s)++"'? Type y if so."
                  answer <- liftIO $ getLine
                  case answer of
                    "y" -> exec s
                    "i" -> runInspect s
                    otherwise -> throwError "User decided to abort."

 runInspect :: Statement -> Run ()
 runInspect s = do
    (_, st) <- get
    liftIO $ printEnvironment st
    execCheck s

 printEnvironment :: Env -> IO ()
 printEnvironment = putStrLn . show

 class SmartAssignment a where
  assign :: String -> a -> Statement

 instance SmartAssignment Int where
  assign v i = Assign v (Const (I i))

 instance SmartAssignment Bool where
  assign v b = Assign v (Const (B b))

 instance SmartAssignment Expr where
  assign v e = Assign v e


 class PrettyExpr a b where
  (.*) :: a -> b -> Expr
  (.-) :: a -> b -> Expr


 instance PrettyExpr String String where
  x .* y = (Var x) `Mul` (Var y)
  x .- y = (Var x) `Sub` (Var y)

 instance PrettyExpr String Int where
  x .* y = (Var x) `Mul` (Const (I y))
  x .- y = (Var x) `Sub` (Const (I y))

 run :: Statement -> IO ()
 run program = do result <- runExceptT $ (runStateT $ exec program) ([], Map.empty)
                  case result of
                    Right ((), env) -> return ()
                    Left exn -> System.print ("Uncaught exception: "++exn)

 runX :: [Statement] -> IO ()
 runX s = run2 (s, []) ([], Map.empty)

 run2 :: ([Statement], [Statement]) -> BigEnv -> IO ()
 run2 ([], done) env = return ()
 run2 ((stmt:todo), done) env = do  result <- runExceptT $ (runStateT $ exec stmt) env
                                    case result of
                                      Right ((), nEnv) -> run2 (todo, (stmt:done)) nEnv
                                      Left exn -> System.print ("Uncaught exception: "++exn)