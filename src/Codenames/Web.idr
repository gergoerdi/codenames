module Codenames.Web

import Codenames.Utils
import Js.Dom

%default total
%access public export

node0 : String -> List (HtmlAttribute ev) -> List (Html ev) -> Html ev
node0 = node

table : Vect n (Vect m (Html ev)) -> Html ev
table xs = node0 "table" [] [node0 "tbody" [] $ toList $ map row xs]
  where
    row : Vect m (Html ev) -> Html ev
    row = node0 "tr" [stringAttribute "style" "height: 12ex"] . toList

grid : (h : Nat) -> (w : Nat) -> Vect (h * w) String -> Html ev
grid h w = table . unconcat h w . map (\field => node0 "td" [cssClass field] [])
