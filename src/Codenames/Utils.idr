module Codenames.Utils

import Data.Vect
import Control.ST
import Control.ST.Random

%default total
%access public export

Shuffle : (Type -> Type) -> Type -> Type
Shuffle m a = (rnd : Var) -> ST m a [rnd ::: Random]

backpermute : Vect n a -> Vect n (Fin n) -> Vect n a
backpermute xs = map $ \i => index i xs

unconcat : (n : Nat) -> (m : Nat) -> Vect (n * m) a -> Vect n (Vect m a)
unconcat Z _ [] = []
unconcat (S n) m xs = let (ys, yss) = splitAt m xs in ys :: unconcat n m yss

rotate : Vect (n * m) a -> Vect (n * m) a
rotate = concat . reverse . map reverse . unconcat _ _