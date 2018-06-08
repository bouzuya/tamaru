module Client.Component.App
  ( Input
  , Output
  , Query
  , app
  ) where

import Client.Component.GroupList as GroupList
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (class Eq, class Ord, type (~>), Unit, Void, const, pure, unit)

data Slot = GroupListSlot
derive instance eqGroupListSlot :: Eq Slot
derive instance ordGroupListSlot :: Ord Slot

type State = Unit
data Query a
  = HandleGroupList GroupList.Output a
  | Noop a
type Input = Unit -- input value
type Output = Void -- output message

app :: forall m. H.Component HH.HTML Query Input Output m
app =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
  eval :: Query ~> H.ParentDSL State Query GroupList.Query Slot Output m
  eval (HandleGroupList _ a) = pure a
  eval (Noop a) = pure a

  render :: State -> H.ParentHTML Query GroupList.Query Slot m
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
        , HH.slot
          GroupListSlot
          GroupList.groupList
          { groupList: [] }
          (HE.input HandleGroupList)
        ]
      , HH.footer []
        [ HH.text "bouzuya"
        ]
      ]
    ]
