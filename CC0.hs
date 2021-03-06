{-# LANGUAGE
    MultiParamTypeClasses,
    RankNTypes,
    ScopedTypeVariables,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances,
    IncoherentInstances,
    PolyKinds #-}

class SKI r where
    app :: r (a -> b) -> r a -> r b
    s :: r ((a -> b -> c) -> (a -> b) -> a -> c)
    k :: r (a -> b -> a)
    i :: r (a -> a)

newtype ShowTerm x = ShowTerm { showTerm :: String }

instance SKI ShowTerm where
    app f x = ShowTerm $ "(" ++ showTerm f ++ " " ++ showTerm x ++ ")"
    s = ShowTerm "S"
    k = ShowTerm "K"
    i = ShowTerm "I"

newtype Next repr a b = Next { unNext :: repr (a -> b) }

conv :: SKI repr => repr a -> Next repr b a
conv = Next . app k

instance SKI repr => SKI (Next repr a) where
    app f x = Next $ app (app s $ unNext f) $ unNext x
    s = conv s
    k = conv k
    i = conv i

lam :: SKI repr => (Next repr a a -> Next repr a b) -> repr (a -> b)
lam f = unNext $ f (Next i)

c :: SKI r => r ((a -> b -> c) -> b -> a -> c)
c = lam (\abc -> lam (\b -> lam (\a -> app (app (conv $ conv abc) a) $ conv b)))

main :: IO ()
main = putStrLn $ showTerm c
