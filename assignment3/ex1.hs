import Control.Monad.State
import System.IO

-- -----------------------------------------------------------------------------
-- State + IO example
-- http://www.haskell.org/haskellwiki/Simple_StateT_use
-- -----------------------------------------------------------------------------
main :: IO ()
main = runStateT code [1..] >> return ()

-- layer an infinite list of uniques over the IO monad
code :: StateT [Integer] IO ()
code = do
    x <- pop
    io $ print x
    y <- pop 
    io $ print y
    return ()
    
-- pop the next unique off the stack
pop :: StateT [Integer] IO Integer
pop = do 
    (x:xs) <- get
    put xs
    return x

io :: IO a -> StateT [Integer] IO a
io = liftIO
    