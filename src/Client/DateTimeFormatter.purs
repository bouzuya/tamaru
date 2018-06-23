module Client.DateTimeFormatter
  ( calendarDateBasicFormatter
  , calendarDateExtendedFormatter
  , calendarDateFormatter
  , calendarMonthBasicFormatter
  , calendarMonthFormatter
  , calendarYearBasicFormatter
  , calendarYearFormatter
  , dateFormatter
  , hourBasicFormatter
  , hourFormatter
  , minuteBasicFormatter
  , minuteExtendedFormatter
  , minuteFormatter
  , timeBasicFormatter
  , timeExtendedFormatter
  , timeFormatter
  , toISOString
  ) where

import Data.DateTime (DateTime)
import Data.Formatter.DateTime as DateTimeFormatter
import Data.List as List
import Prelude ((<>))

-- NOTE: The following format is not supported
-- - `YY`
-- - `YYMMDD`
-- - `YY-MM-DD`
-- - `-YYMM`
-- - `-YY-MM`
-- - `-YY`
-- - `--MMDD`
-- - `--MM-DD`
-- - `--MM`
-- - `---DD`
-- - `±YYYYYMMDD`
-- - `±YYYYY-MM-DD`
-- - `±YYYYY-MM`
-- - `±YYYYY`
-- - `±YYY`
-- - `YYYYDDD`
-- - `YYYY-DDD`
-- - `YYDDD`
-- - `YY-DDD`
-- - `-DDD`
-- - `±YYYYYDDD`
-- - `±YYYYY-DDD`
-- - `YYYYWwwD`
-- - `YYYY-Www-D`
-- - `YYYYWww`
-- - `YYYY-Www`
-- - `YYWwwD`
-- - `YY-Www-D`
-- - `YYWww`
-- - `YY-Www`
-- - `-YWwwD`
-- - `-Y-Www-D`
-- - `-YWww`
-- - `-Y-Www`
-- - `-Www`
-- - `-W-D`
-- - `±YYYYYWwwD`
-- - `±YYYYY-Www-D`
-- - `±YYYYYWww`
-- - `±YYYYY-Www`
-- - (time) ...

-- ISO 8601 calendar date complete representation basic format
-- YYYYMMDD
calendarDateBasicFormatter :: DateTimeFormatter.Formatter
calendarDateBasicFormatter =
  List.fromFoldable
  [ DateTimeFormatter.YearFull
  , DateTimeFormatter.MonthTwoDigits
  , DateTimeFormatter.DayOfMonthTwoDigits
  ]

-- ISO 8601 calendar date complete representation extended format
-- YYYY-MM-DD
calendarDateExtendedFormatter :: DateTimeFormatter.Formatter
calendarDateExtendedFormatter =
  List.fromFoldable
  [ DateTimeFormatter.YearFull
  , DateTimeFormatter.Placeholder "-"
  , DateTimeFormatter.MonthTwoDigits
  , DateTimeFormatter.Placeholder "-"
  , DateTimeFormatter.DayOfMonthTwoDigits
  ]

calendarDateFormatter :: DateTimeFormatter.Formatter
calendarDateFormatter = calendarDateExtendedFormatter

-- ISO 8601 calendar month basic format
calendarMonthBasicFormatter :: DateTimeFormatter.Formatter
calendarMonthBasicFormatter =
  List.fromFoldable
  [ DateTimeFormatter.YearFull
  , DateTimeFormatter.Placeholder "-"
  , DateTimeFormatter.MonthTwoDigits
  ]

calendarMonthFormatter :: DateTimeFormatter.Formatter
calendarMonthFormatter = calendarMonthBasicFormatter

-- ISO 8601 year basic format
calendarYearBasicFormatter :: DateTimeFormatter.Formatter
calendarYearBasicFormatter =
  List.fromFoldable
  [ DateTimeFormatter.YearFull
  ]

calendarYearFormatter :: DateTimeFormatter.Formatter
calendarYearFormatter = calendarYearBasicFormatter

dateFormatter :: DateTimeFormatter.Formatter
dateFormatter = calendarDateExtendedFormatter

-- ISO 8601 time hour basic format
-- hhmm
hourBasicFormatter :: DateTimeFormatter.Formatter
hourBasicFormatter =
  List.fromFoldable
  [ DateTimeFormatter.Hours24
  ]

hourFormatter :: DateTimeFormatter.Formatter
hourFormatter = hourBasicFormatter

-- ISO 8601 time minute basic format
-- hhmm
minuteBasicFormatter :: DateTimeFormatter.Formatter
minuteBasicFormatter =
  List.fromFoldable
  [ DateTimeFormatter.Hours24
  , DateTimeFormatter.MinutesTwoDigits
  ]

-- ISO 8601 time minute extended format
-- hh:mm
minuteExtendedFormatter :: DateTimeFormatter.Formatter
minuteExtendedFormatter =
  List.fromFoldable
  [ DateTimeFormatter.Hours24
  , DateTimeFormatter.Placeholder ":"
  , DateTimeFormatter.MinutesTwoDigits
  ]

minuteFormatter :: DateTimeFormatter.Formatter
minuteFormatter = minuteExtendedFormatter

-- ISO 8601 time complete representation basic format
-- hhmmss
timeBasicFormatter :: DateTimeFormatter.Formatter
timeBasicFormatter =
  List.fromFoldable
  [ DateTimeFormatter.Hours24
  , DateTimeFormatter.MinutesTwoDigits
  , DateTimeFormatter.SecondsTwoDigits
  ]

-- ISO 8601 time complete representation extended format
-- hh:mm:ss
timeExtendedFormatter :: DateTimeFormatter.Formatter
timeExtendedFormatter =
  List.fromFoldable
  [ DateTimeFormatter.Hours24
  , DateTimeFormatter.Placeholder ":"
  , DateTimeFormatter.MinutesTwoDigits
  , DateTimeFormatter.Placeholder ":"
  , DateTimeFormatter.SecondsTwoDigits
  ]

timeFormatter :: DateTimeFormatter.Formatter
timeFormatter = timeExtendedFormatter

-- `Date.prototype.toString`
-- "YYYY-MM-DDThh:mm:ss.sssZ"
toISOString :: DateTime -> String
toISOString dt =
  DateTimeFormatter.format iso8601Formatter dt
  where
  iso8601Formatter
    = calendarDateExtendedFormatter
    <> List.fromFoldable [ DateTimeFormatter.Placeholder "T" ]
    <> timeExtendedFormatter
    <> List.fromFoldable
      [ DateTimeFormatter.Placeholder "."
      , DateTimeFormatter.Milliseconds
      , DateTimeFormatter.Placeholder "Z"
      ]
