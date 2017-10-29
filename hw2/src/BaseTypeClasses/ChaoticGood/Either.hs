{-# LANGUAGE NoImplicitPrelude #-}

module BaseTypeClasses.ChaoticGood.Either where

import           Prelude (Applicative, Foldable, Functor, Traversable, fmap,
                          foldMap, mempty, pure, sequenceA, ($), (<$>), (<*>))

data Either e a = Left e | Right a

instance Functor (Either e) where
    fmap _ (Left e)  = Left e
    fmap f (Right a) = Right $ f a

instance Applicative (Either e) where
    pure = Right

    (Left e) <*> _ = Left e
    _ <*> (Left e) = Left e
    (Right f) <*> (Right a) = Right $ f a

instance Foldable (Either e) where
    foldMap _ (Left _)  = mempty
    foldMap f (Right a) = f a

instance Traversable (Either e) where
    sequenceA (Left e)  = pure $ Left e
    sequenceA (Right f) = Right <$> f

-- PROOFS

{- Applicative3: pure f <*> pure x = pure (f x)
    pure f <*> pure x   === Right f <*> Right x                                 -- definition of pure
                        === Right $ f x                                         -- definition of <*>

    pure (f x)          === Right $ f x                                         -- definition of pure
-}

{- Applicative4: u <*> pure y = pure ($ y) <*> u
    Left e <*> pure y === Left e                                                -- definition of <*>

    pure ($ y) <*> Left e === Left e                                            -- definition of <*>

    Right f <*> pure y  === Right f <*> Right y                                 -- definition of pure
                        === Right $ f y

    pure ($ y) <*> Right f  === Right ($ y) <*> Right f                         -- definition of pure
                            === Right $ f y                                     -- definition of <*>
-}
