module Client.Component.GroupList
  ( Input
  , Output
  , Query
  , groupList
  ) where

import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Void, const, id, map, pure)
import Server.Model (Group)

type State = { groupList :: Array Group }
data Query a = Noop a
type Input = { groupList :: Array Group }
type Output = Void

groupList :: forall m. H.Component HH.HTML Query Input Output m
groupList =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where
  eval :: Query ~> (H.ComponentDSL State Query Output m)
  eval (Noop a) = pure a

  render :: State -> H.ComponentHTML Query
  render state =
    HH.select
    [ HP.classes [ ClassName "group-list" ] ]
    ( map
      (\group ->
        HH.option
        [ HP.classes [ ClassName "group-list-item" ] ]
        [ HH.text group.id ]
      )
      state.groupList
    )
