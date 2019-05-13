module Common.Component.GroupList
  ( Input
  , Output(..)
  , Query
  , groupList
  ) where

import Prelude

import Common.Model (Group, GroupId)
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { groupList :: Array Group, selected :: Maybe GroupId }
data Query a
  = Select String a
type Input = { groupList :: Array Group }
data Output
  = Selected String

groupList :: forall m. H.Component HH.HTML Query Input Output m
groupList =
  H.component
    { initialState: (\i -> { groupList: i.groupList, selected: Nothing })
    , render
    , eval
    , receiver: const Nothing
    }
  where
  eval :: Query ~> (H.ComponentDSL State Query Output m)
  eval (Select value next) = do
    s <- H.get
    case find (\({ id }) -> eq id value) s.groupList of
      Nothing -> pure next
      Just group -> do
        _ <- H.modify (_ { selected = Just group.id })
        _ <- H.raise (Selected group.id)
        pure next

  render :: State -> H.ComponentHTML Query
  render state =
    HH.select
    [ HP.classes [ ClassName "group-list" ]
    , HE.onValueChange (HE.input Select)
    ]
    ( map
      (\group ->
        HH.option
        [ HP.classes [ ClassName "group-list-item" ]
        , HP.selected (eq (Just group.id) state.selected)
        , HP.value group.id
        ]
        [ HH.text group.id ]
      )
      state.groupList
    )
