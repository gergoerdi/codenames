module Codenames.Duet.Web

import Codenames.Utils
import Codenames.Web

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

data Event = NewSeed Integer | SwitchPlayer Player

record PageState where
    constructor MkPageState
    fields : Player -> Fields
    player : Player
    seed : Integer

Gui : (Dom m) => Type
Gui {m} = DomRef {m} () (const PageState) (const Event) ()

render : () -> PageState -> Html Event
render () ps@(MkPageState fields side _) = div [cssClass "container"]
  [ grid Height Width $ fields side
  , div [cssClass "buttons"]
    [ button (playerBtn Player1 [cssClass "left", onclick $ SwitchPlayer Player2 ]) "Player 1"
    , div [cssClass "mid"]
      [ node0 "label" [ stringAttribute "for" "seed"] [text "Seed:"]
      , NewSeed <$> seedInput (seed ps)
      ]
    , button (playerBtn Player2 [cssClass "right", onclick $ SwitchPlayer Player1 ]) "Player 2"
    ]
  ]
  where
    playerBtn : Player -> List (HtmlAttribute a) -> List (HtmlAttribute a)
    playerBtn s attrs = if s == player ps then attrs else (cssClass "hidden")::attrs

exec : Shuffle ASync (Player -> Fields) -> (dom : Var) -> (rnd : Var) -> Event -> ST ASync () [rnd ::: Random, dom ::: Gui {m = ASync}]
exec shuffle dom rnd ev = case ev of
  NewSeed seed => do
    write rnd seed
    fields <- shuffle rnd
    ps <- domGet dom
    let ps' = record{ fields = fields, seed = seed, player $= other } ps
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
    seedNum <- do
        now <- lift . liftJS_IO $ jscall "new Date().getTime()" (JS_IO Int)
        pure $ cast now `mod` 10000
    rnd <- new seedNum
    dom <- do
        fields <- shuffle rnd
        let side = Player1
        initBody [] render () (MkPageState fields side seedNum)

    pageLoop shuffle dom rnd

    clearDom dom
    delete rnd

export runPage : Shuffle ASync (Player -> Fields) -> JS_IO ()
runPage = setASync_ . run . page
