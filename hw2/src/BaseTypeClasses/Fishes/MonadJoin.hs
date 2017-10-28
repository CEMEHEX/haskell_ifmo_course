{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BaseTypeClasses.Fishes.MonadJoin where

import           Prelude (Functor, fmap, ($), (.), (<$>))

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

    {- LAWS
        1. m >>= return    ≡ m                              -- first mpnad law
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
    -}

instance (Functor m, MonadJoin m) => Monad m where
    return = returnJoin

    m >>= f = join $ f <$> m

instance (Functor m, MonadJoin m) => MonadFish m where
    returnFish = returnJoin

    f >=> g = join . fmap g . f -- f >=> g = \a -> join $ g <$> f a

--ASSUMPTION: f . g === \x -> f (g x)

-- PROOFS:

-- 1) instance (Functor m, MonadJoin m) => Monad m

{-  m >>= return ≡ m
   m >>= return === join (fmap return m)                        -- definition of >>=
                === join (fmap returnJoin m)                    -- definition of return
                === (join . fmap returnJoin) m                  -- assumption about (.)
                === id m                                        -- second monadJoin law
                === m                                           -- definition of id
-}

-- 2) instance (Functor m, MonadJoin m) => MonadFish m

{- f >=> returnFish ≡ f
    f >=> returnFish    === f >=> returnJoin                       -- definition of returnFish
                        === \a -> join $ fmap returnJoin (f a)     -- definition of >=>
                        === \a -> (join . fmap returnJoin) (f a)   -- assumption about (.)
                        === \a -> id (f a)                         -- second monadJoin law
                        === \a -> f a                              -- definition of id
                        === f                                      -- η - reduction
-}
