{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module BaseTypeClasses.Fishes.MonadFunctor where

import           Prelude ((.))

class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

    {- LAWS
        1. m >>= return    ≡ m                              -- first monad law
        2. return a >>= f  ≡ f a                            -- second monad law
        3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)        -- third monad law
    -}

class Functor f where
    fmap :: (a -> b) -> f a -> f b

    {- LAWS
        1. fmap id      === id
        2. fmap (p . q) === (fmap p) . (fmap q)
    -}

instance Monad m => Functor m where
    fmap f m = m >>= (return . f)              -- Equality for fmap

-- ASSUMPTION: (f . g) === \x -> f (g x)
-- PROOFS
{- 1. fmap id === id
        fmap id === \m -> m >>= (return . id)                               -- definition of fmap
                === \m -> m >>= return                                      -- laws of composition
                === \m -> m                                                 -- first monad law
                === id                                                      -- definition of id
-}

{- 2. fmap (p . q) === (fmap p) . (fmap q)
    (fmap p) . (fmap q) === \m -> (fmap p) (fmap q m)                       -- assumption about composition
                        === \m -> fmap p (m >>= (return . q))               -- definition of fmap
                        === \m -> m >>= (return . q) >>= (return . p)       -- definition of fmap
                        === \m -> m >>= \x ->
                                return (q x) >>= (return  . p)              -- third monad law + assumption about (.)
                        === \m -> m >>= \x -> return (p (q x))              -- second monad law
                        === \m -> m >>= (p . q)                             -- assumption about (.)
                        === fmap (p . q)                                    -- definiiton of fmap
-}
