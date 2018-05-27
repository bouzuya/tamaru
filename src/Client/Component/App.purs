module Client.Component.App
  ( Input
  , Output
  , Query
  , app
  ) where

import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, const, pure, unit)

type State = Unit
data Query a = Noop a
type Input = Unit -- input value
type Output = Unit -- output message

app :: forall m. H.Component HH.HTML Query Input Output m
app =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
  eval :: Query ~> (H.ComponentDSL State Query Output m)
  eval (Noop a) = pure a

  render :: State -> H.ComponentHTML Query
  render _ =
    HH.html []
    [ HH.head []
      [ HH.title []
        [ HH.text "tamaru" ]
      ]
    , HH.body []
      [ HH.header []
        [ HH.h1 []
          [ HH.text "tamaru" ]
        ]
      , HH.div
        [ HP.classes
          [ ClassName "body" ]
        ]
        [ HH.p []
          [ HH.text "body" ]
        ]
      , HH.footer []
        [ HH.text "bouzuya"
        ]
      ]
    ]
