module Common.Component.DataInput
  ( Input
  , Output(..)
  , Query
  , dataInput
  ) where

import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.Event.Event (Event, preventDefault)
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..), liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, bind, const, pure)

type State = { output :: String, value :: String } -- output for debug
data Query a
  = Change String a
  | Submit Event a
type Input = Unit
data Output
  = DataAdded String

dataInput :: forall e. H.Component HH.HTML Query Input Output (Aff (dom :: DOM | e))
dataInput =
  H.component
    { initialState: (const { output: "", value: "" })
    , render
    , eval
    , receiver: const Nothing
    }
  where
  eval :: Query ~> H.ComponentDSL State Query Output (Aff (dom :: DOM | e))
  eval (Change value next) = do
    _ <- H.modify (_ { value = value })
    pure next
  eval (Submit event q) = do
    _ <- liftEff (preventDefault event)
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
