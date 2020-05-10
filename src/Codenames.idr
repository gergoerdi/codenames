module Codenames

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
