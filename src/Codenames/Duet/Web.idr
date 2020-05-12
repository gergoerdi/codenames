module Codenames.Duet.Web

import Codenames.Utils

import Control.ST
import Control.ST.ImplicitCall
import Control.ST.Random
import Js.Dom

public export Width : Nat
Width = 5

public export Height : Nat
Height = 5

public export Fields : Type
Fields = Vect (Width * Height) String

public export data Player = Player1 | Player2

Eq Player where
  Player1 == Player1 = True
  Player2 == Player2 = True
  _ == _ = False

other : Player -> Player
other Player1 = Player2
other Player2 = Player1

parseHash : String -> Maybe (Player, Integer)
parseHash s = Nothing

makeHash : Player -> Integer -> String
makeHash Player1 seed = "P1/" ++ show seed
makeHash Player2 seed = "P2/" ++ show seed

getHash : ASync String
getHash = liftJS_IO $ jscall "document.location.hash" _

setHash : String -> ASync String
setHash = liftJS_IO . jscall "document.location.hash = %0" _

data Event = NewSeed Integer | SwitchPlayer Player

record PageState where
    constructor MkPageState
    fields : Player -> Fields
    player : Player
    seed : Integer

export node0 : String -> List (HtmlAttribute ev) -> List (Html ev) -> Html ev
node0 = node

total table : Vect n (Vect m (Html ev)) -> Html ev
table xs = node0 "table" [] [node0 "tbody" [] $ toList $ map row xs]
  where
    row : Vect m (Html ev) -> Html ev
    row = node0 "tr" [stringAttribute "style" "height: 12ex"] . toList

grid : PageState -> Vect Height (Vect Width (Html ev))
grid (MkPageState fields side _) = unconcat Height Width $ map square (fields side)
  where
    square field = node0 "td" [cssClass field] []

Gui : (Dom m) => Type
Gui {m} = DomRef {m} () (const PageState) (const Event) ()

render : () -> PageState -> Html Event
render () ps = div [cssClass "container"]
  [ table $ grid ps
  , div [cssClass "buttons"]
    [ button (playerBtn Player1 [cssClass "left", onclick $ SwitchPlayer Player2 ]) "Player 1"
    , div [cssClass "mid"]
      [ node0 "label" [ stringAttribute "for" "seed"] [text "Seed:"]
      , numInput
          [ stringAttribute "id" "seed"
          , propertyAttribute "style" "width: 5ex"
          , stringAttribute "value" $ show $ seed ps
          , onchange (NewSeed . cast . substr 0 4)
          -- TODO: manage selection
          -- , eventListenerAttribute "onfocus" (\this => ?select)
          -- , eventListenerAttribute "onmouseup" (\this => pure False)
          ]
      ]
    , button (playerBtn Player2 [cssClass "right", onclick $ SwitchPlayer Player1 ]) "Player 2"
    ]
  ]
  where
    playerBtn : Player -> List (HtmlAttribute a) -> List (HtmlAttribute a)
    playerBtn s attrs = if s == player ps then attrs else (cssClass "hidden")::attrs

    numAttrs : List (InputAttribute a)
    numAttrs =
      [ propertyAttribute "type" "text"
      , stringAttribute "maxlength" "4"
      , stringAttribute "size" "4"
      , stringAttribute "inputmode" "numeric"
      , propertyAttribute "pattern" "[0-9]{4}"
      ]

    numInput : List (InputAttribute a) -> Html a
    numInput attrs = input (attrs ++ numAttrs)

exec : Shuffle ASync (Player -> Fields) -> (dom : Var) -> (rnd : Var) -> Event -> ST ASync () [rnd ::: Random, dom ::: Gui {m = ASync}]
exec shuffle dom rnd ev = case ev of
  NewSeed seed => do
    write rnd seed
    fields <- shuffle rnd
    ps <- domGet dom
    let ps' = record{ fields = fields, seed = seed, player $= other } ps
    lift $ setHash $ makeHash (player ps') seed
    domPut dom ps'
  SwitchPlayer p => do
    ps <- domGet dom
    let ps' = record{ player = p} ps
    domPut dom ps'

pageLoop
    : Shuffle ASync (Player -> Fields)
    -> (dom : Var)
    -> (rnd : Var)
    -> ST ASync () [rnd ::: Random, dom ::: Gui {m = ASync}]
pageLoop shuffle dom rnd = do
    ev <- getInput dom
    exec shuffle dom rnd ev
    pageLoop shuffle dom rnd

page : Shuffle ASync (Player -> Fields) -> ST ASync () []
page shuffle = do
    now <- lift . liftJS_IO $ jscall "new Date().getTime()" (JS_IO Int)
    hash <- lift getHash

    let init = fromMaybe (Player1, cast now `mod` 10000) $ parseHash hash
    let player = fst init
    let seed = snd init
    lift $ setHash $ makeHash player seed

    rnd <- new seed
    dom <- do
        fields <- shuffle rnd
        let ps = MkPageState fields player seed
        initBody [] render () ps

    pageLoop shuffle dom rnd

    clearDom dom
    delete rnd

export runPage : Shuffle ASync (Player -> Fields) -> JS_IO ()
runPage = setASync_ . run . page
