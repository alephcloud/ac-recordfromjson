{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, OverlappingInstances, OverloadedStrings, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}

module AlephCloud.RecordFromJSON where

import Control.Applicative
import Data.Aeson
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BL
import Data.Data
import Data.EitherR(fmapL)
import Data.List(isPrefixOf)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import Debug.Trace
import Safe

{-

This is machinery for generic FromJSON instances with better error
handling than the derived instances for Record data types, e.g. data
types with one constructor and some labelled arguments such as:

  data R = R {r1 :: String, r2 :: Int} deriving (Data, Show, Typeable)

where the FromJSON instance can simply be:

  instance FromJSON R where parseJSON = recordFromJSON R

which will essentially generate the following FromJSON instance but with better error mesages:

  instance FromJSON R where
    parseJSON (Object o) = R <$> o .: "r1" <*> o .: "r2" <*> o .: "r3"

There is also a myDecode function which is eitherDecode with a
slightly nicer error message for malformed JSON.

-}

-- Examples
--
data R = R {r1 :: String, r2 :: Int} deriving (Data, Show, Typeable)
data R1 = R1 {r11 :: R, r12 :: Maybe String, r22 :: [Int]} deriving (Data, Show, Typeable)

instance FromJSON R where parseJSON = recordFromJSON R 
instance FromJSON R1 where parseJSON = recordFromJSON R1 


-- Generate a A.Parser for a given record type
--
--   recordFromJSON should be given the constructor for the record.
--
class RecordFromJSON a b | a -> b where
  recordFromJSON :: a -> Value -> A.Parser b

instance (Data b, RcdFromJSON a b) => RecordFromJSON a b where
  recordFromJSON r = rcdFromJSON (getRcdFields (undefined :: b)) (pure r)

-- Have to pass in the record's field names which is handled by the RecordFromJSON instance.
--
class RcdFromJSON a b | a -> b where
  rcdFromJSON :: [T.Text] -> A.Parser a -> Value -> A.Parser b

instance (ab ~ (a -> b), FromJSON a, RcdFromJSON b c) => RcdFromJSON ab c where
  rcdFromJSON [] _ _ = failParse "Internal error, rcdFromJSON ran out of field names"

  rcdFromJSON (f:fs) p obj@(Object o) = rcdFromJSON fs (p <*> errFld (o .: f)) obj
    where errFld = A.modifyFailure $ \x ->
            -- This is very, very hacky. The space is inserted to prevent later isPrefixOf matches.
            if "key" `isPrefixOf` x then (" " <> x) else (("in field " <> T.unpack f <> ": ") <> x)
  
  rcdFromJSON _ _ v = failParse ("expecting object, received " <> show v)

instance RcdFromJSON a a where
  rcdFromJSON [] c _ = c
  rcdFromJSON fs _ _ = failParse ("Internal error, rcdFromJSON did not consume all field names: " <> show fs)


failParse msg = A.modifyFailure (const msg) empty

getRcdFields :: Data a => a -> [T.Text]
getRcdFields = map T.pack . headNote "getRcdFields" . map constrFields . dataTypeConstrs . dataTypeOf

-- a decodeEither which replaces generic failure message with "Malformed JSON"
--
myDecode :: FromJSON a => BL.ByteString -> Either String a
myDecode = fmapL msg . eitherDecode
    where msg x = if "Failed" `isPrefixOf` x || "not enough input" == x then "Malformed JSON" else x

