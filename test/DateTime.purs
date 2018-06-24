module Test.DateTime (tests) where

import Bouzuya.DateTime (Date, Day, DayOfYear, Hour, Millisecond, Minute, Month, Second, Time(..), Weekday, Year, exactDate)
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (($))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: forall e. TestSuite e
tests = suite "Bouzuya.DateTime" do
  test "Types" do
    let
      year2018 :: Year
      year2018 = unsafePartial $ fromJust $ toEnum 2018
      month1 :: Month
      month1 = unsafePartial $ fromJust $ toEnum 1
      week1 :: Weekday
      week1 = unsafePartial $ fromJust $ toEnum 1
      dayOfMonth1 :: Day
      dayOfMonth1 = unsafePartial $ fromJust $ toEnum 1
      dayOfYear1 :: DayOfYear
      dayOfYear1 = unsafePartial $ fromJust $ toEnum 1
      date20180101 :: Date
      date20180101 = unsafePartial $ fromJust $ exactDate year2018 month1 dayOfMonth1
      hour0 :: Hour
      hour0 = unsafePartial $ fromJust $ toEnum 0
      minute0 :: Minute
      minute0 = unsafePartial $ fromJust $ toEnum 0
      second0 :: Second
      second0 = unsafePartial $ fromJust $ toEnum 0
      millisecond0 :: Millisecond
      millisecond0 = unsafePartial $ fromJust $ toEnum 0
      time0 :: Time
      time0 = Time hour0 minute0 second0 millisecond0
    Assert.assert "type" true
