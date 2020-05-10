module Codenames

import Control.ST

%access public export
%default total

data Side = Red | Blue

Eq Side where
  Red == Red = True
  Blue == Blue = True
  _ == _ = False

data Field : Type where
    Bystander : Field
    Assassin : Field
    Agent : Maybe Side -> Field

Shuffle : (Type -> Type) -> Type -> Type
Shuffle m a = (seed : Var) -> ST m a [seed ::: State Integer]
