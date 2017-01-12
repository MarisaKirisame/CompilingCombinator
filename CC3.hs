{-# LANGUAGE
    MultiParamTypeClasses,
    RankNTypes,
    ScopedTypeVariables,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances,
    IncoherentInstances,
    PolyKinds #-}

class Comb r where
    app :: r (a -> b) -> r a -> r b
    s :: r ((a -> b -> c) -> (a -> b) -> a -> c)
    k :: r (a -> b -> a)
    i :: r (a -> a)
    b :: r ((b -> c) -> (a -> b) -> (a -> c))
    c :: r ((a -> b -> c) -> (b -> a -> c))
    w :: r ((a -> a -> b) -> (a -> b))

class Arith r where
    int :: Int -> r Int
    add :: r (Int -> Int -> Int)

newtype ShowTerm x = ShowTerm { showTerm :: String }

instance Comb ShowTerm where
    app f x = ShowTerm $ "(" ++ showTerm f ++ " " ++ showTerm x ++ ")"
    s = ShowTerm "S"
    k = ShowTerm "K"
    i = ShowTerm "I"
    b = ShowTerm "B"
    c = ShowTerm "C"
    w = ShowTerm "W"

instance Arith ShowTerm where
    int x = ShowTerm $ show x
    add = ShowTerm "add"

newtype Eval x = Eval { eval :: x }

instance Comb Eval where
    app f x = Eval $ eval f $ eval x
    s = Eval (\abc ab a -> abc a (ab a))
    k = Eval const
    i = Eval id
    b = Eval (\f g x -> f (g x))
    c = Eval (\f a b -> f b a)
    w = Eval (\f x -> f x x)

instance Arith Eval where
    int = Eval
    add = Eval (+)

data Next repr a b = Fast (repr b) | Slow (repr (a -> b))

unNext :: Comb repr => Next repr a b -> repr (a -> b)
unNext (Slow x) = x
unNext (Fast x) = app k x

instance Comb repr => Comb (Next repr a) where
    app (Fast f) (Fast x) = Fast $ app f x
    app (Slow f) (Slow x) = Slow $ app (app s f) x
    app (Slow f) (Fast x) = (Slow $ app (app c f) x)
    app (Fast f) (Slow x) = (Slow $ app (app b f) x)
    s = Fast s
    k = Fast k
    i = Fast i
    b = Fast b
    c = Fast c
    w = Fast w

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
    
lam :: forall r a b. Comb r =>
 ((forall k. NT (Next r a) k => k a) -> (Next r a) b) -> r (a -> b)
lam f = unNext $ f (conv (Slow i :: Next r a a))

double :: Comb r => Arith r => r (Int -> Int)
double = lam (\i -> app (app add i) i)

iota :: Comb r => r ((((x -> y -> z) -> (x -> y) -> (x -> z)) -> (b -> c -> b) -> a) -> a)
iota = lam (\f -> app (app f (lam (\f -> lam (\x -> lam (\arg -> app (app f arg) (app x arg)))))) (lam (\x -> (lam (\y -> x)))))

main :: IO ()
main = putStrLn $ showTerm iota
