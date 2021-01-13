module Main where

import Prelude

import Effect (Effect, foreachE)
import Effect.Console (log)
import Data.String.Common (joinWith)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Functor

main :: Effect Unit
main = do
  let
    x :: Maybe Int
    x = pure 5
  log $ show $ x
  -- log $ fromMaybe "no" $ join (Just (Just "Hello"))
