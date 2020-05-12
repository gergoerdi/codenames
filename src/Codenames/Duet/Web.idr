module Codenames.Duet.Web

import Codenames.Utils

import Js.Dom

import Control.ST
import Control.ST.ImplicitCall
import Control.ST.LiftEffect

import Effects
import Effect.Random
import Effect.Random.Shuffle

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
    [ button (playerBtn Player1 [cssClass "left", onclick $ SwitchPlayer Player2 ]) "Side A"
    , div [cssClass "mid"]
      [ node0 "label" [ stringAttribute "for" "seed"] [text "Seed:"]
      , numInput
          [ stringAttribute "id" "seed"
          , propertyAttribute "style" "width: 5ex"
          , stringAttribute "value" $ show $ seed ps
          ]
      ]
    , button (playerBtn Player2 [cssClass "right", onclick $ SwitchPlayer Player1 ]) "Side B"
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

exec : Shuffle (Player -> Fields) -> (dom : Var) -> (seed : Var) -> Event -> ST ASync () [seed ::: State Integer, dom ::: Gui {m = ASync}]
exec shuffle dom seed ev = case ev of
  NewSeed n => do
    write seed n
    fields <- call $ liftEff seed shuffle
    ps <- domGet dom
    let ps' = record{ fields = fields, seed = n} ps
    domPut dom ps'
  SwitchPlayer p => do
    ps <- domGet dom
    let ps' = record{ player = p} ps
    domPut dom ps'

pageLoop
    : Shuffle (Player -> Fields)
    -> (dom : Var)
    -> (seed : Var)
    -> ST ASync () [seed ::: State Integer, dom ::: Gui {m = ASync}]
pageLoop shuffle dom seed = do
    ev <- getInput dom
    exec shuffle dom seed ev
    pageLoop shuffle dom seed

page : Shuffle (Player -> Fields) -> ST ASync () []
page shuffle = do
    seedNum <- do
        now <- lift . liftJS_IO $ jscall "new Date().getTime()" (JS_IO Int)
        pure $ cast now `mod` 10000
    seed <- new seedNum
    dom <- do
        fields <- call $ liftEff seed $ shuffle
        let side = Player1
        initBody [] render () (MkPageState fields side seedNum)

    pageLoop shuffle dom seed

    clearDom dom
    delete seed

export runPage : Shuffle (Player -> Fields) -> JS_IO ()
runPage = setASync_ . run . page
