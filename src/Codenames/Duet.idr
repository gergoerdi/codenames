module Main

import Codenames
import Codenames.Web

import Data.Vect
import Data.Fin
import Js.Dom

import Control.ST
import Control.ST.ImplicitCall
import Effects
import Effect.Random
import Effect.Random.Shuffle
import Control.ST.LiftEffect

backpermute : Vect n a -> Vect n (Fin n) -> Vect n a
backpermute xs = map $ \i => index i xs

rotate : Vect (n * m) a -> Vect (n * m) a
rotate = concat . reverse . map reverse . unconcat _ _

generate : (Monad m) => Shuffle m (Side -> Fields)
generate seed = do
    idxs <- call $ liftEff seed $ shuffle indices

    let (shared, nonshared) = splitAt 3 idxs
    let (red, nonred) = splitAt 6 nonshared
    let (nonblue, blue) = splitAt (25 - (3 + 6)) nonshared

    red' <- call $ liftEff seed $ shuffle nonred
    blue' <- call $ liftEff seed $ shuffle nonblue

    pure $ \side => case side of
      Red => backpermute template $ shared ++ red ++ red'
      Blue => rotate $ backpermute template $ shared ++ blue ++ blue'
  where
    indices : Vect (Width * Height) (Fin 25)
    indices = fromList [0..24]

    template : Vect (Width * Height) Field
    template =
      replicate 12 (Agent Nothing) ++
      replicate 3 Assassin ++
      replicate _ Bystander

main : JS_IO ()
main = runPage generate
