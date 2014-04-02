import Control.Monad.State
import qualified Data.Map as M

-- Compute fibonacci number n by using memoization.
fib :: Integer -> State (M.Map Integer Integer) Integer
fib 1 = return 1
fib 2 = return 1
fib n = do
        s <- get
        case M.lookup (n - 2) s of
            Just f2  -> do 
                        case M.lookup (n - 1) s of
                            Just f1 -> do
                                       modify (M.insert n (f1 + f2))
                                       return $ f1 + f2
                            Nothing -> do
                                       f1 <- fib (n - 1)
                                       modify (M.insert n (f1 + f2))
                                       return $ f1 + f2
            Nothing -> do 
                       f2 <- fib (n - 2)
                       f1 <- fib (n - 1)
                       modify (M.insert n (f1 + f2)) 
                       return $ f1 + f2
                       

-- Produce the fibonacci sequence. Ultra fast.
fibs = evalState (mapM fib [1..]) (M.fromList [])
