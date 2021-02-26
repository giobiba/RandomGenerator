module RandomLib where

newtype State s a =
    State { runState :: s -> (a, s)}

-- fmap : Functor (State s) => (a -> b) -> State s a -> State s b
instance Functor (State s) where
    fmap f st = State $ \s -> let (a, s') = runState st s
                              in (f a, s')

instance Applicative (State s) where
    pure v = State $ \s -> (v, s)

    stateFunc <*> stateA = State $ \s -> let (f, s') = runState stateFunc s
                                             (a, s'') = runState stateA s'
                                         in (f a, s'')

-- (<*>) :: (s -> (a -> b,s)) -> (s -> (a, s)) -> (s -> (b, s))
-- (<*>) :: Applicative (State s) => State s (a -> b) -> State s a -> State s b

instance Monad (State s) where
    return v = State $ \s -> (v, s)

    ma >>= k =
        State $ \s -> let (va, s') = runState ma s
                          in runState (k va) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (() , f s)
