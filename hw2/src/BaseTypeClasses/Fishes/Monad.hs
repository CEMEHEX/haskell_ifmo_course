{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BaseTypeClasses.Fishes.Monad where

import           Prelude (id, (.))

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

instance Monad m => MonadFish m where
    returnFish = return

    f >=> g = (>>= g) . f --f >=> g = \a -> f a >>= g

instance Monad m => MonadJoin m where
    returnJoin = return

    join = (>>= id)      -- join m = m >>= id

--ASSUMPTION: f . g === \x -> f (g x)

-- PROOFS:

-- 1) instance Monad m => MonadFish m

{- f >=> returnFish ≡ f
    f >=> returnFish === f >=> return                         -- definition of returnFish
                     === \a -> f a >>= return                 -- definition of >=>
                     === \a -> f a                            -- first monad law
                     === f                                    -- η - reduction
-}
{- (f >=> g) >=> h  ≡ f >=> (g >=> h)
    (f >=> g) >=> h === (\a -> f a >>= g) >=> h               -- definition of >=>
                    === \b -> (\a -> f a >>= g) b >>= h       -- definition of >=>
                    === \b -> f b >>= g >>= h                 -- β - reduction
                    === \b -> f b >>= \x -> g x >>= h         -- third monad law
                    === \b -> f b >>= (g >=> h)               -- definition of >=>
                    === f >=> (g >=> h)                       -- definition of >=>
-}

-- 2) instance Monad m => MonadJoin m

{- join . pure ≡ id
    join . pure === join . return                           -- assuming return === pure
                === (\m -> m >>= id) . return               -- definition of join
                === \x -> (\m -> m >>= id) (return x)       -- assumption about composition
                === \x -> return x >>= id                   -- β - reduction
                === \x -> id x                              -- second monad law
                === id                                      -- η - reduction
-}
-- TODO
{- join . fmap join ≡ join . join
   join . fmap join === \x -> join (fmap join x)                        -- assumption about (.)
                    === \x -> join (x >>= (return . join))              -- equality for fmap
                    === \x -> join (x >>= \y -> return (join y))        -- assumption about (.)
                    === \x -> join (x >>= \y -> return (y >>= id))      -- definition of join
                    === \x -> (x >>= \y -> return (y >>= id)) >>= id    -- definition of join

-}
