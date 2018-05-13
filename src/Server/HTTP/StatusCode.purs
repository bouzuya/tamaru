-- See:
-- RFC 7231
-- https://tools.ietf.org/html/rfc7231
--
-- Hypertext Transfer Protocol (HTTP) Status Code Registry
-- https://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml
module Server.HTTP.StatusCode
  ( Code
  , ReasonPhrase
  , ResponseClass(..)
  , StatusCode(..)
  , fromInt
  , getResponseClass
  , isClientError
  , isInformational
  , isRedirection
  , isServerError
  , isSuccessful
  , status100
  , status101
  , status102
  , status103
  , status200
  , status201
  , status202
  , status203
  , status204
  , status205
  , status206
  , status207
  , status208
  , status226
  , status300
  , status301
  , status302
  , status303
  , status304
  , status305
  , status306
  , status307
  , status308
  , status400
  , status401
  , status402
  , status403
  , status404
  , status405
  , status406
  , status407
  , status408
  , status409
  , status410
  , status411
  , status412
  , status413
  , status414
  , status415
  , status416
  , status417
  , status421
  , status422
  , status423
  , status424
  , status425
  , status426
  , status427
  , status428
  , status429
  , status430
  , status431
  , status451
  , status500
  , status501
  , status502
  , status503
  , status504
  , status505
  , status506
  , status507
  , status508
  , status509
  , status510
  , status511
  ) where

import Data.Boolean (otherwise)
import Data.Eq (class Eq, (==))
import Data.HeytingAlgebra ((&&))
import Data.Maybe (Maybe(..))
import Data.Ord ((<=))
import Data.Show (class Show)

type Code = Int
type ReasonPhrase = String
data StatusCode = StatusCode Code ReasonPhrase

instance eqStatusCode :: Eq StatusCode where
  eq (StatusCode a _) (StatusCode b _) = a == b

instance showStatusCode :: Show StatusCode where
  show (StatusCode _ s) = s

data ResponseClass
  = Informational -- 1xx
  | Successful -- 2xx
  | Redirection -- 3xx
  | ClientError -- 4xx
  | ServerError -- 5xx

derive instance eqResponseClass :: Eq ResponseClass

instance showResponseClass :: Show ResponseClass where
  show Informational = "1xx (Informational)"
  show Successful = "2xx (Successful)"
  show Redirection = "3xx (Redirection)"
  show ClientError = "4xx (Client Error)"
  show ServerError = "5xx (Server Error)"

fromInt :: Int -> Maybe StatusCode
fromInt 100 = Just status100
fromInt 101 = Just status101
fromInt 102 = Just status102
fromInt 103 = Just status103
fromInt 200 = Just status200
fromInt 201 = Just status201
fromInt 202 = Just status202
fromInt 203 = Just status203
fromInt 204 = Just status204
fromInt 205 = Just status205
fromInt 206 = Just status206
fromInt 207 = Just status207
fromInt 208 = Just status208
fromInt 226 = Just status226
fromInt 300 = Just status300
fromInt 301 = Just status301
fromInt 302 = Just status302
fromInt 303 = Just status303
fromInt 304 = Just status304
fromInt 305 = Just status305
fromInt 306 = Just status306
fromInt 307 = Just status307
fromInt 308 = Just status308
fromInt 400 = Just status400
fromInt 401 = Just status401
fromInt 402 = Just status402
fromInt 403 = Just status403
fromInt 404 = Just status404
fromInt 405 = Just status405
fromInt 406 = Just status406
fromInt 407 = Just status407
fromInt 408 = Just status408
fromInt 409 = Just status409
fromInt 410 = Just status410
fromInt 411 = Just status411
fromInt 412 = Just status412
fromInt 413 = Just status413
fromInt 414 = Just status414
fromInt 415 = Just status415
fromInt 416 = Just status416
fromInt 417 = Just status417
fromInt 421 = Just status421
fromInt 422 = Just status422
fromInt 423 = Just status423
fromInt 424 = Just status424
fromInt 425 = Just status425
fromInt 426 = Just status426
fromInt 427 = Just status427
fromInt 428 = Just status428
fromInt 429 = Just status429
fromInt 430 = Just status430
fromInt 431 = Just status431
fromInt 451 = Just status451
fromInt 500 = Just status500
fromInt 501 = Just status501
fromInt 502 = Just status502
fromInt 503 = Just status503
fromInt 504 = Just status504
fromInt 505 = Just status505
fromInt 506 = Just status506
fromInt 507 = Just status507
fromInt 508 = Just status508
fromInt 509 = Just status509
fromInt 510 = Just status510
fromInt 511 = Just status511
fromInt _ = Nothing -- Unassigned or invalid

getResponseClass :: StatusCode -> Maybe ResponseClass
getResponseClass (StatusCode code _)
  | 100 <= code && code <= 199 = Just Informational
  | 200 <= code && code <= 299 = Just Successful
  | 300 <= code && code <= 399 = Just Redirection
  | 400 <= code && code <= 499 = Just ClientError
  | 500 <= code && code <= 599 = Just ServerError
  | otherwise = Nothing

isInformational :: StatusCode -> Boolean
isInformational s = case getResponseClass s of
  Just Informational -> true
  _ -> false

isSuccessful :: StatusCode -> Boolean
isSuccessful s = case getResponseClass s of
  Just Successful -> true
  _ -> false

isRedirection :: StatusCode -> Boolean
isRedirection s = case getResponseClass s of
  Just Redirection -> true
  _ -> false

isClientError :: StatusCode -> Boolean
isClientError  s = case getResponseClass s of
  Just ClientError -> true
  _ -> false

isServerError :: StatusCode -> Boolean
isServerError s = case getResponseClass s of
  Just ServerError -> true
  _ -> false

status100 :: StatusCode
status100 = StatusCode 100 "Continue"

status101 :: StatusCode
status101 = StatusCode 101 "Switching Protocols"

status102 :: StatusCode
status102 = StatusCode 102 "Processing"

status103 :: StatusCode
status103 = StatusCode 103 "Early Hints"

status200 :: StatusCode
status200 = StatusCode 200 "OK"

status201 :: StatusCode
status201 = StatusCode 201 "Created"

status202 :: StatusCode
status202 = StatusCode 202 "Accepted"

status203 :: StatusCode
status203 = StatusCode 203 "Non-Authoritative Information"

status204 :: StatusCode
status204 = StatusCode 204 "No Content"

status205 :: StatusCode
status205 = StatusCode 205 "Reset Content"

status206 :: StatusCode
status206 = StatusCode 206 "Partial Content"

status207 :: StatusCode
status207 = StatusCode 207 "Multi-Status"

status208 :: StatusCode
status208 = StatusCode 208 "Already Reported"

status226 :: StatusCode
status226 = StatusCode 226 "IM Used"

status300 :: StatusCode
status300 = StatusCode 300 "Multiple Choices"

status301 :: StatusCode
status301 = StatusCode 301 "Moved Permanently"

status302 :: StatusCode
status302 = StatusCode 302 "Found"

status303 :: StatusCode
status303 = StatusCode 303 "See Other"

status304 :: StatusCode
status304 = StatusCode 304 "Not Modified"

status305 :: StatusCode
status305 = StatusCode 305 "Use Proxy"

status306 :: StatusCode
status306 = StatusCode 306 "(Unused)"

status307 :: StatusCode
status307 = StatusCode 307 "Temporary Redirect"

status308 :: StatusCode
status308 = StatusCode 308 "Permanent Redirect"

status400 :: StatusCode
status400 = StatusCode 400 "Bad Request"

status401 :: StatusCode
status401 = StatusCode 401 "Unauthorized"

status402 :: StatusCode
status402 = StatusCode 402 "Payment Required"

status403 :: StatusCode
status403 = StatusCode 403 "Forbidden"

status404 :: StatusCode
status404 = StatusCode 404 "Not Found"

status405 :: StatusCode
status405 = StatusCode 405 "Method Not Allowed"

status406 :: StatusCode
status406 = StatusCode 406 "Not Acceptable"

status407 :: StatusCode
status407 = StatusCode 407 "Proxy Authentication Required"

status408 :: StatusCode
status408 = StatusCode 408 "Request Timeout"

status409 :: StatusCode
status409 = StatusCode 409 "Conflict"

status410 :: StatusCode
status410 = StatusCode 410 "Gone"

status411 :: StatusCode
status411 = StatusCode 411 "Length Required"

status412 :: StatusCode
status412 = StatusCode 412 "Precondition Failed"

status413 :: StatusCode
status413 = StatusCode 413 "Payload Too Large"

status414 :: StatusCode
status414 = StatusCode 414 "URI Too Long"

status415 :: StatusCode
status415 = StatusCode 415 "Unsupported Media Type"

status416 :: StatusCode
status416 = StatusCode 416 "Range Not Satisfiable"

status417 :: StatusCode
status417 = StatusCode 417 "Expectation Failed"

status421 :: StatusCode
status421 = StatusCode 421 "Misdirected Request"

status422 :: StatusCode
status422 = StatusCode 422 "Unprocessable Entity"

status423 :: StatusCode
status423 = StatusCode 423 "Locked"

status424 :: StatusCode
status424 = StatusCode 424 "Failed Dependency"

status425 :: StatusCode
status425 = StatusCode 425 "Unassigned"

status426 :: StatusCode
status426 = StatusCode 426 "Upgrade Required"

status427 :: StatusCode
status427 = StatusCode 427 "Unassigned"

status428 :: StatusCode
status428 = StatusCode 428 "Precondition Required"

status429 :: StatusCode
status429 = StatusCode 429 "Too Many Requests"

status430 :: StatusCode
status430 = StatusCode 430 "Unassigned"

status431 :: StatusCode
status431 = StatusCode 431 "Request Header Fields Too Large"

status451 :: StatusCode
status451 = StatusCode 451 "Unavailable For Legal Reasons"

status500 :: StatusCode
status500 = StatusCode 500 "Internal Server Error"

status501 :: StatusCode
status501 = StatusCode 501 "Not Implemented"

status502 :: StatusCode
status502 = StatusCode 502 "Bad Gateway"

status503 :: StatusCode
status503 = StatusCode 503 "Service Unavailable"

status504 :: StatusCode
status504 = StatusCode 504 "Gateway Timeout"

status505 :: StatusCode
status505 = StatusCode 505 "HTTP Version Not Supported"

status506 :: StatusCode
status506 = StatusCode 506 "Variant Also Negotiates"

status507 :: StatusCode
status507 = StatusCode 507 "Insufficient Storage"

status508 :: StatusCode
status508 = StatusCode 508 "Loop Detected"

status509 :: StatusCode
status509 = StatusCode 509 "Unassigned"

status510 :: StatusCode
status510 = StatusCode 510 "Not Extended"

status511 :: StatusCode
status511 = StatusCode 511 "Network Authentication Required"
