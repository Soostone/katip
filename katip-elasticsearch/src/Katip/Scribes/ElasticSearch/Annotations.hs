{-# LANGUAGE OverloadedStrings #-}
module Katip.Scribes.ElasticSearch.Annotations
    ( TypeAnnotated(..)
    -- * Exported for benchmarking
    , deannotateValue
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import qualified Data.Foldable       as FT
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific     (isFloating)
import           Data.Text           (Text)
import qualified Data.Text           as T
-------------------------------------------------------------------------------
import           Katip
-------------------------------------------------------------------------------


-- | Represents a value that can be converted to and from JSON that
-- will type annotate object keys when serializing and strip them out when deserializating
newtype TypeAnnotated a = TypeAnnotated {
      typeAnnotatedValue :: a
    }

instance ToJSON a => ToJSON (TypeAnnotated a) where
  toJSON = annotateValue . toJSON . typeAnnotatedValue


instance ToObject a => ToObject (TypeAnnotated a)


instance FromJSON a => FromJSON (TypeAnnotated a) where
  parseJSON v = TypeAnnotated <$> parseJSON (deannotateValue v)


instance LogItem a => LogItem (TypeAnnotated a) where
  payloadKeys v (TypeAnnotated x) = case payloadKeys v x of
    AllKeys -> AllKeys
    -- Take the key selection, overlap it with the actual keys
    -- produced and annotate them
    SomeKeys ks -> let o = toObject x
                       oInFocus = HM.fromList $ zip ks (repeat Null)
                       final = annotateKeys $ HM.intersection o oInFocus
                   in SomeKeys $ HM.keys final

-------------------------------------------------------------------------------
-- Conversion Functions
-------------------------------------------------------------------------------


annotateValue :: Value -> Value
annotateValue (Object o) = Object $ annotateKeys o
annotateValue (Array a)  = Array (annotateValue <$> a)
annotateValue x          = x


annotateKeys :: Object -> Object
annotateKeys = HM.fromList . map go . HM.toList
  where
    go (k, Object o) = (k, Object $ annotateKeys o)
    go (k, Array a)  = (k, Array (annotateValue <$> a))
    go (k, s@(String _)) = (k <> stringAnn, s)
    go (k, n@(Number sci)) = if isFloating sci
                             then (k <> doubleAnn, n)
                             else (k <> longAnn, n)
    go (k, b@(Bool _)) = (k <> booleanAnn, b)
    go (k, Null) = (k <> nullAnn, Null)


deannotateValue :: Value -> Value
deannotateValue (Object o) = Object $ deannotateKeys o
deannotateValue (Array a)  = Array (deannotateValue <$> a)
deannotateValue x          = x


deannotateKeys :: Object -> Object
deannotateKeys = HM.fromList . map go . HM.toList
  where
    go (k, Object o) = (k, Object $ deannotateKeys o)
    go (k, Array a)  = (k, Array (deannotateValue <$> a))
    go (k, v)        = (fromMaybe k k', v)
      where
        k' = FT.asum (stripSuffix <$> suffixes)
        suffixes = [stringAnn, doubleAnn, longAnn, booleanAnn, nullAnn]
        stripSuffix suffix = T.stripSuffix suffix k


-------------------------------------------------------------------------------
-- Annotation Constants
-------------------------------------------------------------------------------


stringAnn :: Text
stringAnn = "::s"

doubleAnn :: Text
doubleAnn = "::d"

longAnn :: Text
longAnn = "::l"

booleanAnn :: Text
booleanAnn = "::b"

nullAnn :: Text
nullAnn = "::n"
