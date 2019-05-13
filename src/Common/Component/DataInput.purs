module Common.Component.DataInput
  ( Input
  , Output(..)
  , Query
  , dataInput
  ) where

import Prelude

import Effect.Aff (Aff)
import Web.Event.Event (Event, preventDefault)
import Halogen (ClassName(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { output :: String, value :: String } -- output for debug
data Query a
  = Change String a
  | Submit Event a
type Input = String
data Output
  = DataAdded String

dataInput :: H.Component HH.HTML Query Input Output Aff
dataInput =
  H.component
    { initialState: (const { output: "", value: "" })
    , render
    , eval
    , receiver: HE.input Change
    }
  where
  eval :: Query ~> H.ComponentDSL State Query Output Aff
  eval (Change value next) = do
    _ <- H.modify (_ { value = value })
    pure next
  eval (Submit event q) = do
    _ <- liftEffect (preventDefault event)
    value <- H.gets _.value
    _ <- H.raise (DataAdded value)
    pure q

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
    [ HP.classes [ ClassName "data-input" ]
    ]
    [ HH.form [ HE.onSubmit (HE.input Submit) ]
      [ HH.label [ HP.classes [ ClassName "value" ] ]
        [ HH.span [ HP.classes [ ClassName "label" ] ]
          [ HH.text "value" ]
        , HH.span [ HP.classes [ ClassName "value" ] ]
          [ HH.input
            [ HE.onValueChange (HE.input Change)
            , HP.name "value"
            , HP.value state.value
            ]
          ]
        ]
      , HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text "OK" ]
      ]
    , HH.span [ HP.classes [ ClassName "output" ] ] [ HH.text state.output ]
    ]
