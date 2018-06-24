module Test.DayOfYear (tests) where

import Client.DateTime.Component.DayOfYear (canonicalDateFromDayOfYear, dayOfYear, exactDateFromDayOfYear, lastDayOfYear)
import Data.Date (Month(..), canonicalDate)
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: forall e. TestSuite e
tests = suite "Client.DateTime.Component.DayOfYear" do
  let
    year2018 = (unsafePartial (fromJust (toEnum 2018)))
    year2020 = (unsafePartial (fromJust (toEnum 2020)))
    dayOfYear2 = (unsafePartial (fromJust (toEnum 2)))
    dayOfYear365 = (unsafePartial (fromJust (toEnum 365)))
    dayOfYear366 = (unsafePartial (fromJust (toEnum 366)))
    date20180102 =
      canonicalDate
        year2018
        January
        (unsafePartial (fromJust (toEnum 2)))
  test "canonicalDateFromDayOfYear" do
    Assert.equal
      date20180102
      (canonicalDateFromDayOfYear year2018 dayOfYear2)
  test "dayOfYear" do
    Assert.equal (dayOfYear date20180102) dayOfYear2
  test "exactDateFromDayOfYear" do
    Assert.equal
      (Just date20180102)
      (exactDateFromDayOfYear year2018 dayOfYear2)
    -- TODO: Nothing
  test "lastDayOfYear" do
    Assert.equal dayOfYear365 (lastDayOfYear year2018)
    Assert.equal dayOfYear366 (lastDayOfYear year2020)

