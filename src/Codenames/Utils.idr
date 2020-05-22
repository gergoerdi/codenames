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

private plusS : (x : Nat) -> (y : Nat) -> x + (S y) = S (x + y)
plusS Z y = Refl
plusS (S x) y = cong (plusS x y)

partitionLen :
  (p : a -> Bool) -> (xs : Vect len a) ->
  let (ys, ns) = partition p xs in fst ys + fst ns = len
partitionLen p [] = Refl
partitionLen {len = S _} p (x :: xs) with (partition p xs) proof eq
  | ((ylen ** ys), (nlen ** ns)) with (replace (sym eq) $ partitionLen p xs)
    | prf with (p x)
      | True = cong prf
      | False = trans (plusS ylen nlen) $ cong prf

inspect : {b : a -> Type} -> (f : (x : a) -> b x) -> (x : a) -> (y ** f x = y)
inspect f x = (f x ** Refl)
