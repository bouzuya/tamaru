module Client.Component.ClientRoot
  ( Input
  , Output
  , Query
  , clientRoot
  ) where

import Client.Component.DataInput as DataInput
import Client.Component.DataList as DataList
import Client.Component.GroupList as GroupList
import Control.Monad.Aff (Aff)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import DOM (DOM)
import Data.Array as Array
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen (ClassName(..), lift)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, append, bind, const, pure, unit, ($), (==))
import Server.Model (Group)

type ChildQuery = Coproduct3 GroupList.Query DataInput.Query DataList.Query
type ChildSlot = Either3 Unit Unit Unit

type State =
  { groupList :: Array Group
  , selectedGroup :: Maybe Group
  }
data Query a
  = HandleDataInput DataInput.Output a
  | HandleDataList DataList.Output a
  | HandleGroupList GroupList.Output a
  | Noop a
type Input = { groupList :: Array Group } -- input value
type Output = Void -- output message

update :: forall a. (a -> Boolean) -> a -> Array a -> Maybe (Array a)
update f x xs = do
  i <- Array.findIndex f xs
  Array.updateAt i x xs

upsert :: forall a. (a -> Boolean) -> a -> Array a -> Array a
upsert f x xs = fromMaybe (append xs [x]) (update f x xs)

clientRoot :: forall e. H.Component HH.HTML Query Input Output (Aff (dom :: DOM | e))
clientRoot =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output (Aff (dom :: DOM | e))
  eval (HandleDataInput (DataInput.DataAdded value) next) = do
    -- TODO: validation
    { groupList, selectedGroup } <- H.get
    case selectedGroup of
      Nothing -> pure next
      Just group -> do
        _ <- runMaybeT do
          -- TODO: today
          let
            today = "2018-06-22"
            newData = upsert (\i -> i.id == today) { id: today, value } group.data
          let newGroup = { id: group.id, data: newData }
          newGroupList <- MaybeT $ pure $ update (\i -> i.id == group.id) newGroup groupList
          lift $ H.modify (_ { groupList = newGroupList, selectedGroup = Just newGroup })
        pure next
  eval (HandleDataList _ a) = pure a
  eval (HandleGroupList (GroupList.Selected groupId) next) = do
    { groupList } <- H.get
    let group = Array.find (\{ id } -> id == groupId) groupList
    _ <- H.modify (_ { selectedGroup = group })
    pure next
  eval (Noop a) = pure a

  initialState :: Input -> State
  initialState { groupList } =
    { groupList
    , selectedGroup: Array.head groupList
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (dom :: DOM | e))
  render state =
    HH.div
    [ HP.classes [ ClassName "body" ] ]
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
      DataInput.dataInput
      unit
      (HE.input HandleDataInput)
    , HH.slot'
      CP.cp3
      unit
      DataList.dataList
      { dataList: maybe [] (\g -> g.data) state.selectedGroup }
      (HE.input HandleDataList)
    ]
