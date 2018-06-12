module Client.Component.App
  ( Input
  , Output
  , Query
  , app
  ) where

import Client.Component.Body as Body
import Data.Either.Nested (Either1)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, absurd, const, id, pure, unit)
import Server.Model (Group)

type ChildQuery = Coproduct1 Body.Query
type ChildSlot = Either1 Unit

type State = { groupList :: Array Group }
data Query a
  = Noop a
type Input = { groupList :: Array Group } -- input value
type Output = Void -- output message

app :: forall m. H.Component HH.HTML Query Input Output m
app =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where
  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output m
  eval (Noop a) = pure a

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state =
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
        [ HH.slot' CP.cp1 unit Body.body state absurd ]
      , HH.footer []
        [ HH.text "bouzuya"
        ]
      ]
    ]
