module Server.Path
  ( parsePath'
  ) where

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.String (Pattern(..))
import Data.String as String
import Prelude (not, (<<<), (<>), (==))

type Path = String
type NormalizedPath = String
type ParsedPath = Array String

normalizePath :: ParsedPath -> NormalizedPath
normalizePath parsedPath = "/" <> (intercalate "/" parsedPath)

parsePath :: Path -> ParsedPath
parsePath path =
  Array.filter (not <<< String.null) (String.split (Pattern "/") path)

parsePath' :: Path -> Either NormalizedPath ParsedPath
parsePath' pathname =
  let
    parsedPath = parsePath pathname
    normalizedPath = normalizePath parsedPath
  in
    if pathname == normalizedPath
    then Right parsedPath
    else Left normalizedPath
