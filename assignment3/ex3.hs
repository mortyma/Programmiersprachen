import Control.Monad.State
 
data Vars = Vars {
   var1 :: Int,
   var2 :: Float
}
 
type MyState a = StateT Vars IO a
type Selector a = (MyState a, a -> MyState ())
 
s1 :: Selector Int
s1 = (gets var1, \x -> modify (\vs -> vs {var1 = x}))
 
s2 :: Selector Float
s2 = (gets var2, \x -> modify (\vs -> vs {var2 = x}))
 
sel :: Selector a -> MyState a
sel = fst
 
mods :: Selector a -> (a -> a) -> MyState ()
mods (gf,uf) mfun = do st <- gf
                       uf (mfun st)
 
sample :: MyState ()
sample = do a <- sel s1
            mods s2 (\x -> x * (fromIntegral a))
            b <- sel s2
            liftIO $ print b
 
main = runStateT sample (Vars 2 1.3)