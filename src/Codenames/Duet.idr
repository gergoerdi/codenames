module Main

import Codenames
import Codenames.Web
import Data.Vect
import Js.Dom

fields : Vect (Width * Height) Field
fields =
    replicate 6 (Agent (Just Red)) ++
    replicate 6 (Agent (Just Blue)) ++
    replicate 3 (Agent Nothing) ++
    replicate 3 Assassin ++
    replicate 7 Bystander

-- 15 = 9 + 9 - 3 = 6 + 6 + 3

main : JS_IO ()
main = runPage fields
