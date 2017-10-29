{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BaseTypeClasses.Fishes.MonadFish where

import           Prelude (const, id, ($))

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

    {- LAWS
        1. m >>= return    ≡ m                              -- first monad law
        2. return a >>= f  ≡ f a                            -- second monad law
        3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)        -- third monad law
    -}

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

    {- LAWS
        1. f >=> returnFish ≡ f
        2. returnFish >=> f ≡ f
        3. (f >=> g) >=> h  ≡ f >=> (g >=> h)
    -}

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

    {- LAWS
        1. join . pure            ≡ id
        2. join . fmap returnJoin ≡ id
        3. join . fmap join       ≡ join . join
        4* join . fmap (fmap f)   ≡ fmap f . join
    -}

instance MonadFish m => Monad m where
    return = returnFish

    m >>= f = const m >=> f $ ()

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish

    join m = const m >=> id $ () -- OR join = (>>= id)  (using instance for Monad)

--ASSUMPTION: f . g === \x -> f (g x)

-- PROOFS:

-- 1) instance MonadFish m => Monad m

{- m >>= return ≡ m
    m >>= return === const m >=> return $ ()                       -- definition of >>=
                 === const m >=> returnFish $ ()                   -- definition of return
                 === const m $ ()                                  -- monadFish first law
                 === m                                             -- definition of const
-}

-- 2) instance MonadFish m => MonadJoin m

{- join . pure ≡ id
    join . pure === join . returnFish                                            -- assuming pure === returnJoin, returnJoin === returnFish
                === (\m -> (const m >=> id) ()) . returnFish                    -- definition of join
                === \x -> (\m -> (const m >=> id) ()) (returnFish x)            -- assumption about (.)
                === \x -> (const (returnFish x) >=> id) ()                      -- β - reduction
                === \x -> ((\_ -> returnFish x) >=> id) ()                      -- definition of const
                === \x -> (\_ -> id x) ()                                       -- fourth monadFish law
                === \x -> id x                                                  -- β - reduction
                === id                                                          -- η - reduction
-- TODO
fourth monadFish law: (\_ -> returnFish x) >=> f  === \_ -> f x
    (\_ -> returnFish x) >=> f  ===
-}
