{-# LANGUAGE TypeOperators #-}

module OptionalMonads.Partial where

import qualified Control.Category as Cat (Category, id, (.))
import           Control.Monad    ((<=<))
import           Data.Maybe       (fromMaybe, isJust)

data a ~> b = Partial (a -> Maybe b)
            | Defaulted (a ~> b) b

partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total :: (a -> b) -> a ~> b
total = Partial . (Just .)

apply :: (a ~> b) -> a -> Maybe b
apply (Partial f) arg = f arg
apply (Defaulted f def) arg = case apply (getDeepest f) arg of
    Nothing -> Just def
    other   -> other
    where
        getDeepest (Defaulted f' _) = getDeepest f'
        getDeepest f'               = f'

applyOrElse :: (a ~> b) -> a -> b -> b
applyOrElse f arg def = fromMaybe def $ apply f arg

withDefault :: (a ~> b) -> b -> (a ~> b)
withDefault = Defaulted

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt f arg   = isJust $ apply f arg

orElse :: (a ~> b) -> (a ~> b) -> a ~> b
orElse f1 f2 = Partial combination
    where
        combination a = if isDefinedAt f1 a then apply f1 a else apply f2 a

instance Cat.Category (~>) where
    id = total id                             -- (1): id definition
    f1 . f2 = partial $ apply f1 <=< apply f2 -- (2): composition definition

--     lemma1: f === apply (partial f)
{- Proof

    f === apply (partial f)
      === apply (Partial f)              -- definition of partial
      === f                              -- definition of apply
-}

-- assumption1:  f1 <=< f2 === \a -> f2 a >>= f1

-- 1) f . id === f
--    (apply (f . id) === apply f)
{- Proof
    f . id === partial $ apply f <=< apply id               -- (2): definition of composition
           === partial $ apply f <=< apply $ total id       -- (1): definition of id
           === partial $ apply f <=< apply $ Partial Just   -- definition of total
           === partial $ apply f <=< Just                   -- definition of apply
           === partial $ \a -> Just a >>= \x -> (apply f) x -- assumption about <=<
           === partial $ \a -> (apply f) a                  -- definition of >>=
           === partial $ apply f                            -- η - reduction
           === Partial $ apply f                            -- definition of partial
           ==> apply (f . id) === apply f                   -- lemma1
-}

-- 2) id . f === f
--    (apply (id . f) === apply f)
{- Proof
    id . f === partial $ apply id <=< apply f               -- (2): definition of composition
           === partial $ apply (total id) <=< apply f       -- (1): definition of id
           === partial $ apply (Partial Just) <=< apply f   -- definition of total
           === partial $ Just <=< apply f                   -- definition of apply
           === partial $ \a -> (apply f) a >>= \x -> Just x -- assumption about <=<
           === partial $ apply f >>= Just                   -- η - reductions
           === partial $ apply f >>= return                 -- definition of return
           === partial $ apply f                            -- monad law: right identity
           === Partial $ apply f                            -- definition of partial
           ==> apply (id . f) === apply f                   -- lemma1
-}

-- 3) (f1 . f2) . f3 === f1 . (f2 . f3)
{- Proof
    (f1 . f2) . f3 === partial $ apply (partial $ apply f1 <=< apply f2) <=< apply f3    -- (2): definition of composition
                   === partial $ apply (Partial $ apply f1 <=< apply f2) <=< apply f3    -- definition of partial
                   === partial $ (apply f1 <=< apply f2) <=< apply f3                    -- definition of apply
                   === partial $ (\a ->
                                  apply f2 a >>= \x ->
                                  apply f1 x) <=< apply f3                               -- assumption about <=<
                   === partial $ \b ->
                                 apply f3 b >>= \y ->
                                 (\a ->
                                 apply f2 a >>= \x ->
                                 apply f1 x) y                                           -- assumption about <=<
                   === partial $ \b ->
                                 apply f3 b >>= \y ->
                                 apply f2 y >>= \x ->
                                 apply f1 x                                              -- β - reduction
                   === partial $ \a ->
                                 (apply f2 <=< apply f3) a >>= \x ->
                                 apply f1 x                                              -- assumption about <=<
                   === partial $ (apply f1) <=< (apply f2 <=< apply f3)                  -- assumption about <=<
                   === partial $ (apply f1) <=< apply (Partial $ apply f2 <=< apply f3)  -- reverse lemma1
                   === partial $ (apply f1) <=< apply (f2 . f3)                          -- (2): definition of composition
                   === f1 . (f2 . f3)                                                    -- (2): definition of composition
-}
