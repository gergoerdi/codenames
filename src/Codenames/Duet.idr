module Main

import Codenames.Utils
import Codenames.Duet.Web

import Data.Vect
import Data.Fin
import Js.Dom

import Control.ST
import Control.ST.Random
import Control.ST.Random.Shuffle

total generate : (Monad m) => Shuffle m (Player -> Fields)
generate rnd = do
    (shared, nonshared) <- choose rnd 4 indices

    (agents1, nonagents1) <- choose rnd 6 nonshared
    (agents2, nonagents2) <- choose rnd 6 nonagents1

    (assassins1, others1) <- choose rnd 2 nonagents1

    let ((n, k) ** (prf, xs, ys)) = partitionLen (`elem` assassins1) nonagents2
    (assassins2, others2') <- choose rnd 2 (agents1 ++ xs)

    let prf' = trans (sym $ plusAssociative 4 n k) $ cong {f = (+) 4} prf
    let others2 = replace {P = \n => Vect n (Fin 25)} prf' (others2' ++ ys)

    pure $ \player => case player of
      Player1 => backpermute template $ shared ++ agents1 ++ assassins1 ++ others1
      Player2 => rotate $ backpermute template $ shared ++ agents2 ++ assassins2 ++ others2
  where
    indices : Vect (Width * Height) (Fin (Width * Height))
    indices = fromList [0..24]

    template : Vect (Width * Height) String
    template =
      replicate 1 "assassin" ++
      replicate 9 "agent" ++
      replicate 2 "assassin" ++
      replicate _ "bystander"

main : JS_IO ()
main = runPage generate
