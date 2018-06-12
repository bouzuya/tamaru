module Client.Component.App
  ( Input
  , Output
  , Query
  , app
  ) where

import Client.Component.DataList as DataList
import Client.Component.GroupList as GroupList
import Data.Array as Array
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..), maybe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, const, id, pure, unit)
import Server.Model (Group)

type ChildQuery = Coproduct2 GroupList.Query DataList.Query
type ChildSlot = Either2 Unit Unit

type State = { groupList :: Array Group }
data Query a
  = HandleDataList DataList.Output a
  | HandleGroupList GroupList.Output a
  | Noop a
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
  eval (HandleDataList _ a) = pure a
  eval (HandleGroupList _ a) = pure a
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
        [ HH.p []
          [ HH.text "body" ]
        , HH.slot'
          CP.cp1
          unit
          GroupList.groupList
          { groupList: state.groupList }
          (HE.input HandleGroupList)
        , HH.slot'
          CP.cp2
          unit
          DataList.dataList
          { dataList: maybe [] (\g -> g.data) (Array.head state.groupList) }
          (HE.input HandleDataList)
        ]
      , HH.footer []
        [ HH.text "bouzuya"
        ]
      ]
    ]
