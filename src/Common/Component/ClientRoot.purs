module Common.Component.ClientRoot
  ( Input
  , Output
  , Query
  , clientRoot
  , today
  ) where

import Prelude

import Bouzuya.DateTime.Instant (toDateTime)
import Client.Request as Request
import Common.Component.DataInput as DataInput
import Common.Component.DataList as DataList
import Common.Component.GroupList as GroupList
import Common.DateTimeFormatter (calendarDateExtendedFormatter)
import Common.Model (Group)
import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Now (now)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.DateTime (adjust)
import Data.Either (either)
import Data.Either.Nested (Either3)
import Data.Formatter.DateTime as DateTimeFormatter
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.Time.Duration (Hours(..))
import Halogen (ClassName(..), lift, liftEffect)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)

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

today :: Effect String
today
  = map (DateTimeFormatter.format calendarDateExtendedFormatter)
  $ map utcToJst
  $ map toDateTime now
  where
  utcToJst dt = unsafePartial (fromJust (adjust (Hours 9.0) dt))

update :: forall a. (a -> Boolean) -> a -> Array a -> Maybe (Array a)
update f x xs = do
  i <- Array.findIndex f xs
  Array.updateAt i x xs

upsert :: forall a. (a -> Boolean) -> a -> Array a -> Array a
upsert f x xs = fromMaybe (append xs [x]) (update f x xs)

isValid :: String -> Boolean
isValid value =
  either
    (const false)
    (\regex -> Regex.test regex value)
    (Regex.regex "^\\d+(\\.\\d+)?$" noFlags)

clientRoot :: H.Component HH.HTML Query Input Output Aff
clientRoot =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output Aff
  eval (HandleDataInput (DataInput.DataAdded value) next)
    | isValid value = do
        id <- liftEffect today
        { groupList, selectedGroup } <- H.get
        case selectedGroup of
          Nothing -> pure next
          Just group -> do
            _ <- lift $ Request.addData group.id { id, value }
            _ <- runMaybeT do
              let
                newData = upsert (\i -> i.id == id) { id, value } group.data
                newGroup = { id: group.id, data: newData }
              newGroupList <- MaybeT $ pure $ update (\i -> i.id == group.id) newGroup groupList
              lift $ H.modify (_ { groupList = newGroupList, selectedGroup = Just newGroup })
            pure next
    | otherwise = pure next

  eval (HandleDataList _ a) = pure a
  eval (HandleGroupList (GroupList.Selected groupId) next) = do
    { groupList } <- H.get
    let group = Array.find (\g -> g.id == groupId) groupList
    _ <- H.modify (_ { selectedGroup = group })
    pure next
  eval (Noop a) = pure a

  initialState :: Input -> State
  initialState { groupList } =
    { groupList
    , selectedGroup: Array.head groupList
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render state =
    HH.div
    [ HP.classes [ ClassName "body" ] ]
    [ HH.slot'
      CP.cp1
      unit
      GroupList.groupList
      { groupList: state.groupList }
      (HE.input HandleGroupList)
    , HH.slot'
      CP.cp2
      unit
      DataInput.dataInput
      ""
      (HE.input HandleDataInput)
    , HH.slot'
      CP.cp3
      unit
      DataList.dataList
      { dataList: maybe [] (\g -> g.data) state.selectedGroup }
      (HE.input HandleDataList)
    ]
