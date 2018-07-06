module Common.Component.DataList
  ( Input
  , Output
  , Query
  , dataList
  ) where

import Common.Model (Data)
import Data.Array as Array
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), bind, map, pure)

type State = { dataList :: Array Data }
data Query a = HandleInput Input a
type Input = { dataList :: Array Data }
data Output = Void

dataList :: forall m. H.Component HH.HTML Query Input Output m
dataList =
  H.component
    { initialState: (\i -> { dataList: i.dataList })
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where
  eval :: Query ~> (H.ComponentDSL State Query Output m)
  eval (HandleInput i next) = do
    _ <- H.modify (_ { dataList = i.dataList })
    pure next

  render :: State -> H.ComponentHTML Query
  render state =
    HH.ul
    [ HP.classes [ ClassName "data-list" ]
    ]
    ( map
      (\d ->
        HH.li
        [ HP.classes [ ClassName "data-list-item" ] ]
        [ HH.span [ HP.classes [ ClassName "id" ] ] [ HH.text d.id ]
        , HH.span [ HP.classes [ ClassName "separator" ] ] [ HH.text " " ]
        , HH.span [ HP.classes [ ClassName "value" ] ] [ HH.text d.value ]
        ]
      )
      (Array.take 7 (Array.reverse state.dataList))
    )
