{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module MyLenses.BaseLens
    (
      Lens
    , Lens'
    , set
    , view
    , over
    , (.~)
    , (^.)
    , (%~)
    , _1
    , _2
    ) where

import           Data.Functor.Const    (Const (..))
import           Data.Functor.Identity (Identity (..))

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a -- forall f . Functor f => (a -> f a) -> s -> f s

set  :: Lens' s a -> a -> s -> s         -- set    value (setter)
set l val = runIdentity . l (Identity . const val)

view :: Lens' s a -> s -> a              -- lookup value (getter)
view l = getConst . l Const

over :: Lens' s a -> (a -> a) -> s -> s  -- change value (modifier)
over l f s = set l (f $ view l s) s
-- over l f = runIdentity . l (Identity . f)

(.~) :: Lens' s a -> a -> s -> s
(.~) = set

(^.) :: s -> Lens' s a -> a
s ^. l = view l s

(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (, x) <$> f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (x,) <$> f a
