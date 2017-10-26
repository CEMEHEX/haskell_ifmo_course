module Magic where

import           Data.Foldable (fold)

newtype MagicBox t1 t2 a = MagicBox (t1 (t2 a))
    deriving (Show, Eq)

instance (Foldable t1, Foldable t2, Functor t1) => Foldable (MagicBox t1 t2) where
    -- foldMap f (MagicBox t) = foldr mappend mempty $ foldMap f <$> t
    -- foldMap f (MagicBox t) = foldMap id $ foldMap f <$> t
    foldMap f (MagicBox t) = fold $ foldMap f <$> t

instance (Functor t1, Functor t2) => Functor (MagicBox t1 t2) where
    fmap f (MagicBox t) = MagicBox $ fmap f <$> t

instance (Applicative t1, Applicative t2) => Applicative (MagicBox t1 t2) where
    pure = MagicBox . pure . pure

    (MagicBox f) <*> (MagicBox t) = MagicBox $ fmap (<*>) f <*> t

instance (Traversable t1, Traversable t2) =>
         Traversable (MagicBox t1 t2) where
--  sequenceA :: MagicBox t1 t2 (f a) -> f (MagicBox t1 t2 a)
    sequenceA (MagicBox b) = MagicBox <$> sequenceA (sequenceA <$> b)

    -- sequenceA (MagicBox b) = MagicBox <$> step2
    --     where
    --      -- step1 :: t1 (f (t2 a))
    --         step1 = sequenceA <$> b

            -- step2 :: f (t1 (t2 a))
    --         step2 = sequenceA step1
