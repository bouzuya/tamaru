-- https://github.com/slamdata/purescript-halogen-vdom-string-renderer/blob/master/src/Halogen/VDom/DOM/StringRenderer.purs
module Halogen.VDom.DOM.StringRenderer (render) where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Tuple (Tuple(..))
import Foreign (typeOf, unsafeToForeign)
import Foreign.Object as Object
import Halogen.VDom (VDom, ElemName(..))
import Halogen.VDom.DOM.Prop (Prop(..), PropValue)
import Halogen.VDom.StringRenderer as VSR
import Halogen.VDom.StringRenderer.Util (escape)
import Unsafe.Coerce (unsafeCoerce)

render ∷ ∀ i w. (w → String) → VDom (Array (Prop i)) w → String
render renderWidget = VSR.render getTagType renderProps renderWidget

getTagType ∷ ElemName → VSR.TagType
getTagType (ElemName en)
  | Object.member en voidElements = VSR.SelfClosingTag
  | otherwise = VSR.NormalTag

renderProps ∷ ∀ i. Array (Prop i) → String
renderProps = S.joinWith " " <<< A.mapMaybe renderProp

renderProp ∷ ∀ i. Prop i → Maybe String
renderProp = case _ of
  Attribute _ name value → renderAttr name value
  Property name value → renderProperty name value
  Handler _ _ → Nothing
  Ref _ → Nothing

renderAttr ∷ String → String → Maybe String
renderAttr name value = Just $ escape name <> "=\"" <> escape value <> "\""

propNameToAttrName ∷ String → String
propNameToAttrName = case _ of
  "htmlFor" → "for"
  "className" → "class"
  p → p

renderProperty ∷ String → PropValue → Maybe String
renderProperty name prop = case typeOf (unsafeToForeign prop) of
  "string"  → renderAttr name' $ (unsafeCoerce ∷ PropValue → String) prop
  "number"  → renderAttr name' $ show ((unsafeCoerce ∷ PropValue → String) prop)
  "boolean" → Just $ escape name'
  _ → Nothing
  where
  name' = propNameToAttrName name

voidElements :: Object.Object Unit
voidElements =
  Object.fromFoldable
    $ map (flip Tuple unit)
    $ names
  where
  names =
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "isindex" -- legacy XHTML
    , "keygen"
    , "link"
    , "meta"
    , "param"
    , "source"
    , "track"
    , "wbr"
    ]
