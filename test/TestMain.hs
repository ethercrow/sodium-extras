{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.IORef
import Data.List (group)
import qualified FRP.Sodium as S
import qualified FRP.Sodium.Extras as SE
import Test.Tasty.TH
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = $(defaultMainGenerator)

case_uniqueUpdates :: IO ()
case_uniqueUpdates = do
    outputRef <- newIORef []
    push <- S.sync $ do
        (b, push) <- S.newBehavior ' '
        outputEvent <- SE.uniqueUpdates b
        S.listen outputEvent (\c -> modifyIORef outputRef (++ [c]))
        return push
    mapM_ (S.sync . push) input
    actualOutput <- readIORef outputRef
    actualOutput @?= expectedOutput
    where input = "AABBCCDDDAADADA"
          expectedOutput = fmap head (group input)

