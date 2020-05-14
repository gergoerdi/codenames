module Main

import Codenames.Utils
import Codenames.Versus.Web

import Data.Vect
import Data.Fin
import Js.Dom

import Control.ST
import Control.ST.Random
import Control.ST.Random.Shuffle

total generate : (Monad m) => Shuffle m (Fields, String)
generate rnd = do
    extra <- rndSelect rnd ["agentA", "agentB"]
    fields <- shuffle rnd $ extra :: template
    pure (fields, extra)
  where
    template : Vect ((Width * Height) - 1) String
    template =
      replicate 8 "agentA" ++
      replicate 8 "agentB" ++
      replicate 1 "assassin" ++
      replicate _ "bystander"

main : JS_IO ()
main = runPage generate
