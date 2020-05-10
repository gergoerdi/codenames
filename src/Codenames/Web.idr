module Codenames.Web

import Codenames

import Js.Dom
import Control.ST
import Control.ST.ImplicitCall
import Effects
import Effect.Random
import Effect.Random.Shuffle
import Control.ST.LiftEffect

public export Width : Nat
Width = 5

public export Height : Nat
Height = 5

public export Fields : Type
Fields = Vect (Width * Height) Field

data Event = NewSeed Integer | SwitchSide Side

record PageState where
    constructor MkPageState
    fields : Side -> Fields
    side : Side

export node0 : String -> List (HtmlAttribute ev) -> List (Html ev) -> Html ev
node0 = node

total table : Vect n (Vect m (Html ev)) -> Html ev
table xs = node0 "table" [] [node0 "tbody" [] $ toList $ map row xs]
  where
    row : Vect m (Html ev) -> Html ev
    row = node0 "tr" [stringAttribute "style" "height: 12ex"] . toList

export unconcat : (n : Nat) -> (m : Nat) -> Vect (n * m) a -> Vect n (Vect m a)
unconcat Z _ [] = []
unconcat (S n) m xs = let (ys, yss) = splitAt m xs in ys :: unconcat n m yss

grid : PageState -> Vect Height (Vect Width (Html ev))
grid (MkPageState fields side) = unconcat Height Width $ map square (fields side)
  where
    square field = node0 "td" [cssClass cls] []
      where
        cls = case field of
            Bystander => "bystander"
            Assassin => "assassin"
            Agent side' => if maybe True (side ==) side' then "agent" else "bystander"

Gui : (Dom m) => Type
Gui {m} = DomRef {m} () (const PageState) (const Event) ()

render : () -> PageState -> Html Event
render () ps = div [cssClass "container"]
  [ table $ grid ps
  , div [cssClass "buttons"]
    [ button (sideBtn Red [cssClass "left", onclick $ SwitchSide Blue ]) "Red side"
    , div [cssClass "mid"]
      [ node0 "label" [ stringAttribute "for" "seed"] [text "Seed:"]
      , numInput
          [ stringAttribute "id" "seed"
          , propertyAttribute "style" "width: 5ex"
          ]
      ]
    , button (sideBtn Blue [cssClass "right", onclick $ SwitchSide Red ]) "Blue side"
    ]
  ]
  where
    sideBtn : Side -> List (HtmlAttribute a) -> List (HtmlAttribute a)
    sideBtn s attrs = if s == side ps then attrs else (cssClass "hidden")::attrs

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

exec : Shuffle ASync (Side -> Fields) -> (dom : Var) -> (seed : Var) -> Event -> ST ASync () [seed ::: State Integer, dom ::: Gui {m = ASync}]
exec shuffle dom seed ev = case ev of
  NewSeed n => do
    write seed n
    fields <- shuffle seed
    ps <- domGet dom
    let ps' = record{ fields = fields} ps
    domPut dom ps'
  SwitchSide s => do
    ps <- domGet dom
    let ps' = record{ side = s} ps
    domPut dom ps'

pageLoop
    : Shuffle ASync (Side -> Fields)
    -> (dom : Var)
    -> (seed : Var)
    -> ST ASync () [seed ::: State Integer, dom ::: Gui {m = ASync}]
pageLoop shuffle dom seed = do
    ev <- getInput dom
    exec shuffle dom seed ev
    pageLoop shuffle dom seed

page : Shuffle ASync (Side -> Fields) -> ST ASync () []
page shuffle = do
    seed <- do
        now <- lift . liftJS_IO $ jscall "new Date().getTime()" (JS_IO Int)
        new $ cast now
    dom <- do
        fields <- shuffle seed
        let side = Red -- TODO
        initBody [] render () (MkPageState fields side)

    pageLoop shuffle dom seed

    clearDom dom
    delete seed

export runPage : Shuffle ASync (Side -> Fields) -> JS_IO ()
runPage = setASync_ . run . page
