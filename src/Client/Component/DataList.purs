module Client.Component.DataList
  ( Input
  , Output
  , Query
  , dataList
  ) where

import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (type (~>), const, map, pure)
import Server.Model (Data)

type State = { dataList :: Array Data }
data Query a = Noop a
type Input = { dataList :: Array Data }
data Output = Void

dataList :: forall m. H.Component HH.HTML Query Input Output m
dataList =
  H.component
    { initialState: (\i -> { dataList: i.dataList })
    , render
    , eval
    , receiver: const Nothing
    }
  where
  eval :: Query ~> (H.ComponentDSL State Query Output m)
  eval (Noop next) = pure next

  render :: State -> H.ComponentHTML Query
  render state =
    HH.ul
    [ HP.classes [ ClassName "data-list" ]
    ]
    ( map
      (\d ->
        HH.li
        [ HP.classes [ ClassName "data-list-item" ] ]
        [ HH.text d.id ]
      )
      state.dataList
    )
