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

data Next repr a b = Fast (repr b) | Slow (repr (a -> b))

unNext :: SKI repr => Next repr a b -> repr (a -> b)
unNext (Slow x) = x
unNext (Fast x) = app k x

instance SKI repr => SKI (Next repr a) where
    app (Fast f) (Fast x) = Fast $ app f x
    app (Slow f) (Slow x) = Slow $ app (app s f) x
    app (Slow f) (Fast x) = app (Slow f) (Slow $ app k x)
    app (Fast f) (Slow x) = app (Slow $ app k f) (Slow x)
    s = Fast s
    k = Fast k
    i = Fast i

lam :: SKI r => (Next r a a -> Next r a b) -> r (a -> b)
lam f = unNext $ f (Slow i)

c :: SKI r => r ((a -> b -> c) -> b -> a -> c)
c = lam (\abc -> lam (\b -> lam (\a -> app (app (Fast $ Fast abc) a) $ Fast b)))

main :: IO ()
main = putStrLn $ showTerm c
