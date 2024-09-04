{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens
import Data.Map (Map)
import Network.Wai.Handler.Warp
import Network.Wreq
import Servant

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Aeson
import Text.Read (readMaybe)
import GHC.Generics (Generic)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

type PMID = Integer
type Metadata = Object

data Pub = Pub
  { pmid :: PMID
  , count :: Int
  , meta :: Metadata
  } deriving (Show, Generic)

instance ToJSON Pub

newtype PMIDs = PMIDs [PMID]
    deriving Show

instance FromJSON PMIDs where
    parseJSON (String t) = maybe (fail "invalid pmids") (pure . PMIDs) mids
        where tids = T.words t
              mids = sequence (map (readMaybe . T.unpack) tids)
    parseJSON _ = fail "pmids must be passed as a string"

countapiUrl :: PMID -> String
countapiUrl pmid = "https://opencitations.net/index/api/v2/citation-count/pmid:" ++ show pmid

metaapiUrl :: [PMID] -> String
metaapiUrl pmids = "https://opencitations.net/meta/api/v1/metadata/"
    ++ intercalate "__" [ "pmid:" ++ show p | p <- pmids ]

getCitationCount :: PMID -> IO Int
getCitationCount pmid = do
    r <- get (countapiUrl pmid)
    return $ maybe 0 (read . T.unpack) (r ^? responseBody . _Array . traverse . key "count" . _String)

getMeta :: [PMID] -> IO [Object]
getMeta pmids = do
    r <- get (metaapiUrl pmids)
    return $ r ^.. responseBody . _Array . traverse . _Object

getCitationCounts :: PMIDs -> IO [Pub]
getCitationCounts (PMIDs pmids) = do
    counts <- mapConcurrently (\pmid -> (pmid,) <$> getCitationCount pmid) pmids
    metas <- getMeta pmids
    return $ zipWith (\(pmid, c) m -> Pub pmid c m) counts metas

type API = "citations" :> ReqBody '[JSON] PMIDs :> Post '[JSON] [Pub]
      :<|> Raw

server :: FilePath -> Server API
server fp = liftIO . getCitationCounts :<|> serveDirectoryWebApp fp

main :: IO ()
main = do
  args <- getArgs
  case args of
    [portStr, wwwPath] | Just port <- readMaybe portStr -> run port $ serve (Servant.Proxy :: Servant.Proxy API) (server wwwPath)
    _                                          -> error "please specify a port as argument"
