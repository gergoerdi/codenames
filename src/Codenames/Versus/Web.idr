module Codenames.Versus.Web

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

data Event = NewSeed Integer

record PageState where
    constructor MkPageState
    fields : Fields
    firstPlayer : String
    seed : Integer

Gui : (Dom m) => Type
Gui {m} = DomRef {m} () (const PageState) (const Event) ()

render : () -> PageState -> Html Event
render () ps@(MkPageState fields firstPlayer _) = div [cssClass "container"]
  [ div [cssClass (unwords ["firstPlayer", firstPlayer])] []
  , grid Height Width fields
  , div [cssClass (unwords ["firstPlayer", firstPlayer])] []
  , div [cssClass "buttons"]
    [ div [cssClass "mid"]
      [ node0 "label" [ stringAttribute "for" "seed"] [text "Seed:"]
      , NewSeed <$> seedInput (seed ps)
      ]
    ]
  ]

exec : Shuffle ASync (Fields, String) -> (dom : Var) -> (rnd : Var) -> Event -> ST ASync () [rnd ::: Random, dom ::: Gui {m = ASync}]
exec shuffle dom rnd ev = case ev of
  NewSeed seed => do
    write rnd seed
    (fields, firstPlayer) <- shuffle rnd
    ps <- domGet dom
    let ps' = record{ fields = fields, firstPlayer = firstPlayer, seed = seed } ps
    domPut dom ps'

pageLoop
    : Shuffle ASync (Fields, String)
    -> (dom : Var)
    -> (rnd : Var)
    -> ST ASync () [rnd ::: Random, dom ::: Gui {m = ASync}]
pageLoop shuffle dom rnd = do
    ev <- getInput dom
    exec shuffle dom rnd ev
    pageLoop shuffle dom rnd

page : Shuffle ASync (Fields, String) -> ST ASync () []
page shuffle = do
    seedNum <- do
        now <- lift . liftJS_IO $ jscall "new Date().getTime()" (JS_IO Int)
        pure $ cast now `mod` 10000
    rnd <- new seedNum
    dom <- do
        initial <- shuffle rnd
        let fields = fst initial
        let firstPlayer = snd initial
        initBody [] render () (MkPageState fields firstPlayer seedNum)

    pageLoop shuffle dom rnd

    clearDom dom
    delete rnd


export runPage : Shuffle ASync (Fields, String) -> JS_IO ()
runPage = setASync_ . run . page
