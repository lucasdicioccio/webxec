{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import qualified Data.CaseInsensitive as CI
import Data.Aeson as Aeson
import Data.Coerce (coerce)
import qualified Data.List as List
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types (Header, ResponseHeaders, status200, status400)
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import System.Exit (ExitCode(..))
import System.Process.ByteString(readProcessWithExitCode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Options.Generic
import Data.Text.Encoding (decodeUtf8)

data PayloadStyle = NO_PAYLOAD
  | BASIC_PAYLOAD
  | ALL_PAYLOAD
  deriving (Generic, Read, Show)
instance ParseField PayloadStyle
instance ParseRecord PayloadStyle
instance ParseFields PayloadStyle

data ArgStyle = NO_ARGS
  | BASIC_ARGS
  | CONCAT_ARGS
  deriving (Generic, Read, Show)
instance ParseField ArgStyle
instance ParseRecord ArgStyle
instance ParseFields ArgStyle

data Args = Args
  { portNum :: Int <?> "port-num"
  , command :: FilePath <?> "executable"
  , prefixArg :: [String] <?> "extra (prefix) args"
  , suffixArg :: [String] <?> "extra (suffix) args"
  , rspHeader :: [String] <?> "extra response header (key: val)"
  , argStyle :: ArgStyle
  , payloadStyle :: PayloadStyle
  }
  deriving (Generic, Show)

instance ParseRecord Args

data ExtractedReq b = ExtractedReq
  { method :: b
  , path :: b
  , query :: b
  , headers :: [(b, b)]
  , body :: Maybe b
  }
  deriving (Generic, Functor, Show)
instance ToJSON (ExtractedReq Text) where

main :: IO ()
main = do
  args <- getRecord "webxec"
  Warp.run (coerce $ portNum args)
    $ RequestLogger.logStdoutDev
    $ app (args)
  where
    app :: Args -> Wai.Application
    app args req resp = do
      let origmethod = Wai.requestMethod req
      let origpath = Wai.rawPathInfo req
      let origqstring = Wai.rawQueryString req
      let orighdrs = [ (CI.original k, v) | (k,v) <- Wai.requestHeaders req]
      let origbody = fmap LByteString.toStrict (strictRequestBody req)
      content <- case payloadStyle args of
                        NO_PAYLOAD -> pure Nothing
                        BASIC_PAYLOAD -> pure Nothing
                        ALL_PAYLOAD -> fmap Just origbody
      (code, !out, !err) <- runCmd args (ExtractedReq origmethod origpath origqstring orighdrs content)

      let hdrs :: ResponseHeaders
          hdrs = [ ("X-webxec-code", ByteString.pack $ show code)
                 , ("X-webxec-out-length", ByteString.pack $ show $ ByteString.length out)
                 , ("X-webxec-err-length", ByteString.pack $ show $ ByteString.length err)
                 ] <>
                 mkHeaders args
      resp $ Wai.responseLBS status200 hdrs (LByteString.fromStrict $ out <> err)

    runCmd :: Args -> ExtractedReq ByteString -> IO (ExitCode, ByteString, ByteString)
    runCmd args req = do
      let cmdpath = coerce $ command args
      let cmdinput = mkInput args (fmap decodeUtf8 req)
      let cmdargs = mconcat [coerce $ prefixArg args, mkArgs args req, coerce $ suffixArg args]
      readProcessWithExitCode cmdpath cmdargs cmdinput

    mkHeaders :: Args -> ResponseHeaders
    mkHeaders args = fmap mkHeader (coerce $ rspHeader args)

    mkHeader :: String -> Header
    mkHeader str = let (k,v) = List.break (==' ') str in (CI.mk $ ByteString.pack k, ByteString.pack v)

    mkInput :: Args -> ExtractedReq Text -> ByteString
    mkInput args req = case payloadStyle args of
        NO_PAYLOAD -> ""
        BASIC_PAYLOAD -> LByteString.toStrict $ Aeson.encode req
        ALL_PAYLOAD -> LByteString.toStrict $ Aeson.encode req

    mkArgs :: Args -> ExtractedReq ByteString -> [ String ]
    mkArgs args req = case argStyle args of
        NO_ARGS -> []
        BASIC_ARGS -> ByteString.unpack <$>
                    [ method req 
                    , path req
                    , query req
                    ]
        CONCAT_ARGS -> 
                    [ ByteString.unpack
                      $ mconcat
                      [ method req 
                      , " "
                      , path req
                      , query req
                      ]
                    ]
