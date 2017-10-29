{-# LANGUAGE NoImplicitPrelude #-}

module BaseTypeClasses.ChaoticGood.Identity where


import           Prelude (Applicative, Foldable, Functor, Traversable, fmap,
                          foldMap, pure, sequenceA, traverse, ($), (.), (<$>),
                          (<*>))

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f = Identity . f . runIdentity

instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity a) = Identity $ f a

instance Foldable Identity where
    foldMap f = f . runIdentity

instance Traversable Identity where
    sequenceA (Identity f) = Identity <$> f

    traverse f (Identity a) = Identity <$> f a

-- PROOFS

{- Applicative1: pure id <*> v = v
    pure id <*> Identity a  === Identity id <*> Identity a                      -- definition of pure
                            === Identity $ id a                                 -- definition of <*>
                            === Identity a                                      -- definition of id
-}

{- Applicative2: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    pure (.) <*> Identity a <*> Identity b <*> Identity c
        === Identity (.) <*> Identity a <*> Identity b <*> Identity c           -- definition of pure
        === Identity (a .) <*> Identity b <*> Identity c                        -- definition of <*>
        === Identity (a . b) <*> Identity c                                     -- definition of <*>
        === Identity (a . b $ c)                                                -- definition of <*>
        === Identity (a (b (c)))                                                -- definition of (.)
        === Identity a <*> Identity (b (c))                                     -- definition of <*>
        === Identity a <*> (Identity b <*> Identity c)                          -- definition of <*>
-}
