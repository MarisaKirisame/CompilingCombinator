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

class Arith r where
    int :: Int -> r Int
    add :: r (Int -> Int -> Int)

newtype ShowTerm x = ShowTerm { showTerm :: String }

instance SKI ShowTerm where
    app f x = ShowTerm $ "(" ++ showTerm f ++ " " ++ showTerm x ++ ")"
    s = ShowTerm "S"
    k = ShowTerm "K"
    i = ShowTerm "I"

instance Arith ShowTerm where
    int x = ShowTerm $ show x
    add = ShowTerm "add"

newtype Eval x = Eval { eval :: x }

instance SKI Eval where
    app f x = Eval $ eval f $ eval x
    s = Eval (\abc ab a -> abc a (ab a))
    k = Eval const
    i = Eval id

instance Arith Eval where
    int = Eval
    add = Eval (+)

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

instance Arith repr => Arith (Next repr a) where
    int x = Fast (int x)
    add = Fast add

class NT l r where
    conv :: l t -> r t

instance NT l r => NT l (Next r a) where
    conv = Fast . conv

instance NT x x where
    conv = id

--GHC is dumb, I tell ya
instance NT (Next r a) (Next r a) where
    conv = id
    
lam :: forall r a b. SKI r =>
 ((forall k. NT (Next r a) k => k a) -> (Next r a) b) -> r (a -> b)
lam f = unNext $ f (conv (Slow i :: Next r a a))

c :: SKI r => r ((a -> b -> c) -> b -> a -> c)
c = lam (\abc -> lam (\b -> lam (\a -> app (app abc a) b)))

double :: SKI r => Arith r => r (Int -> Int)
double = lam (\i -> app (app add i) i)

main :: IO ()
main = putStrLn $ showTerm double
