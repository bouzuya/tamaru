module Common.Component.ServerRoot
  ( Effect
  , Input
  , Output
  , Query
  , serverRoot
  ) where

import Common.Component.ClientRoot as ClientRoot
import Control.Monad.Aff (Aff)
import DOM (DOM)
import Data.Argonaut (Json, encodeJson)
import Data.Argonaut as Json
import Data.Either.Nested (Either1)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, absurd, const, id, map, pure, unit, ($))
import Server.Model (Group, Data)

type ChildQuery = Coproduct1 ClientRoot.Query
type ChildSlot = Either1 Unit

type State = { groupList :: Array Group }
data Query a
  = Noop a
type Input = { groupList :: Array Group } -- input value
type Output = Void -- output message
type Effect e = (ClientRoot.Effect (dom :: DOM | e))

serverRoot :: forall e. H.Component HH.HTML Query Input Output (Aff (Effect e))
serverRoot =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where
  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output (Aff (Effect e))
  eval (Noop a) = pure a

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (Effect e))
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
        [ HH.slot' CP.cp1 unit ClientRoot.clientRoot state absurd ]
      , HH.footer []
        [ HH.text "bouzuya"
        ]
      , HH.script
        [ HP.attr (AttrName "data-initial-state") (toString state)
        , HP.src "/scripts/index.js" ]
        []
      ]
    ]

  toString :: State -> String
  toString state = Json.stringify $ encodeJson $
    StrMap.fromFoldable
    [ Tuple "groupList" (encodeJson (map encodeJsonGroup state.groupList))
    ]
    where
      encodeJsonGroup :: Group -> Json
      encodeJsonGroup group = encodeJson $
        StrMap.fromFoldable
        [ Tuple "id" (encodeJson group.id)
        , Tuple "data" (encodeJson (map encodeJsonData group.data))
        ]
      encodeJsonData :: Data -> Json
      encodeJsonData d = encodeJson $
        StrMap.fromFoldable
        [ Tuple "id" (encodeJson d.id)
        , Tuple "value" (encodeJson d.value)
        ]

