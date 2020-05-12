module Control.ST.Random.Shuffle

import Control.ST
import Control.ST.Random
import Data.Vect

%access public export
%default total

shuffle : (Monad m) => (rnd : Var) -> Vect n a -> ST m (Vect n a) [rnd ::: Random]
shuffle rnd [] = pure []
shuffle rnd {n = S n} xs@(x :: xs') = do
    k <- rndFin rnd n
    let x' = index k xs
    let (_ :: xs') = replaceAt k x xs
    xs' <- shuffle rnd xs'
    pure (x' :: xs')

choose : (Monad m) => (rnd : Var) -> (n : Nat) -> Vect (n + k) a -> ST m (Vect n a, Vect k a) [rnd ::: Random]
choose rnd n xs = do
    xs <- shuffle rnd xs
    pure $ splitAt n xs
