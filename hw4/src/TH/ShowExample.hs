{-# LANGUAGE TemplateHaskell #-}

module TH.ShowExample
    (
      MyData (..)
    , RecTest (..)
    , InfixTest (..)
    ) where

import           TH.KosherShow (deriveKosherShow)

data MyData = Foo String Int | Bar Int

data RecTest = R {a :: Int, b :: String}

data InfixTest = String :+ Int

deriveKosherShow ''MyData
deriveKosherShow ''RecTest
deriveKosherShow ''InfixTest
