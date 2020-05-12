module Main

import Codenames.Utils
import Codenames.Duet.Web

import Data.Vect
import Data.Fin
import Js.Dom

import Control.ST
import Control.ST.ImplicitCall
import Effects
import Effect.Random
import Effect.Random.Shuffle
import Control.ST.LiftEffect

generate : (Monad m) => Shuffle m (Player -> Fields)
generate seed = do
    idxs <- call $ liftEff seed $ shuffle indices

    let (shared, nonshared) = splitAt 3 idxs
    let (red, nonred) = splitAt 6 nonshared
    let (nonblue, blue) = splitAt (25 - (3 + 6)) nonshared

    red' <- call $ liftEff seed $ shuffle nonred
    blue' <- call $ liftEff seed $ shuffle nonblue

    pure $ \player => case player of
      Player1 => backpermute template $ shared ++ red ++ red'
      Player2 => rotate $ backpermute template $ shared ++ blue ++ blue'
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
