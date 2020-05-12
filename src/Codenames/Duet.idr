module Main

import Codenames.Utils
import Codenames.Duet.Web

import Data.Vect
import Data.Fin
import Js.Dom

import Effects
import Effect.Random
import Effect.Random.Shuffle

generate : Shuffle (Player -> Fields)
generate = do
    (shared, nonshared) <- choose 3 indices

    (agent1, nonagent1) <- choose 6 nonshared
    other1 <- shuffle nonagent1

    (nonagent2, agent2) <- choose (25 - (3 + 6)) nonshared
    other2 <- shuffle nonagent2

    pure $ \player => case player of
      Player1 => backpermute template $ shared ++ agent1 ++ other1
      Player2 => rotate $ backpermute template $ shared ++ agent2 ++ other2
  where
    indices : Vect (Width * Height) (Fin 25)
    indices = fromList [0..24]

    template : Vect (Width * Height) String
    template =
      replicate 12 "agent" ++
      replicate 3 "assassin" ++
      replicate _ "bystander"

main : JS_IO ()
main = runPage generate
