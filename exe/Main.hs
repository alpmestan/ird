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
import qualified Data.ByteString.Lazy as LBS
import Text.HTML.Tree.Lens
import qualified Data.Text.Lazy.Encoding as LT
import Data.Char (isSpace)
import qualified Data.Text.Lazy as LT
import Control.Exception (try, SomeException)

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

-- * OpenCitations queries

ocToken = "b9bb62b6-5d6a-4edc-a912-ef92781b9d30"

oc_countapiUrl :: PMID -> String
oc_countapiUrl pmid = "https://opencitations.net/index/api/v2/citation-count/pmid:" ++ show pmid

oc_metaapiUrl :: [PMID] -> String
oc_metaapiUrl pmids = "https://opencitations.net/meta/api/v1/metadata/"
    ++ intercalate "__" [ "pmid:" ++ show p | p <- pmids ]

getOCCount :: PMID -> IO Int
getOCCount pmid = do
    r <- getWith (defaults & header "Authorization" .~ [ocToken]) (oc_countapiUrl pmid)
    return $ maybe 0 (read . T.unpack) (r ^? responseBody . _Array . traverse . key "count" . _String)

getMeta :: [PMID] -> IO [Object]
getMeta pmids = do
    r <- getWith (defaults & header "Authorization" .~ [ocToken]) (oc_metaapiUrl pmids)
    return $ r ^.. responseBody . _Array . traverse . _Object

getCitationCounts :: PMIDs -> IO [Pub]
getCitationCounts (PMIDs pmids) = do
    counts <- mapConcurrently (\pmid -> (pmid,) <$> getOCCount pmid) pmids
    metas <- getMeta pmids
    return $ zipWith (\(pmid, c) m -> Pub pmid c m) counts metas

-- * Pubmed (HTML) parsing

pget :: String -> IO LBS.ByteString
pget url = view responseBody <$> go
    where go = do
            mres <- try (get url)
            case mres of
                Left (e :: SomeException) -> do
                    putStrLn $ "ERROR url=" ++ url ++ " => " ++ show e
                    go
                Right a -> return a

pmArticlePage :: PMID -> String
pmArticlePage pmid = "https://pubmed.ncbi.nlm.nih.gov/" ++ show pmid ++ "/"

pmCitedInPage :: PMID -> String
pmCitedInPage pmid = "https://pubmed.ncbi.nlm.nih.gov/?linkname=pubmed_pubmed_citedin&from_uid=" ++ show pmid

getPMArticlePage :: PMID -> IO LBS.ByteString
getPMArticlePage pmid = pget (pmArticlePage pmid)

getPMCitedInPage :: PMID -> IO LBS.ByteString
getPMCitedInPage pmid = pget (pmCitedInPage pmid)

scrap :: IO LBS.ByteString -> ([Node] -> Either String a) -> IO (Either String a)
scrap getPage f = go <$> getPage
    where go lbs = f (lbs ^. to LT.decodeUtf8.html)

getPMCount :: PMID -> IO (Either String Int)
getPMCount pmid = scrap (getPMCitedInPage pmid) $ \nodes -> do
    case nodes ^? traverse.allAttributed [("class", "value")].traverse._Text.to (T.replace "," "" . T.replace "." "") of
        Just vstr | Just v <- readMaybe (T.unpack vstr) -> Right v
                  | otherwise -> Left ("Couldn't parse " ++ show vstr ++ " as an integer")
        Nothing -> Left "Couldn't spot class=value tags"

clean :: T.Text -> T.Text
clean = T.replace "  " "" . T.replace "\n" ""

getJournalLink :: [Node] -> Either String String
getJournalLink nodes =
    case nodes ^? traverse.allAttributed [("class", "search-in-nlm-catalog-link dropdown-block-link")].nodeToken._Tag.tagAttrs.ix "href" of
        Just lnk -> Right (T.unpack lnk)
        Nothing  -> Left "Couldn't locate journal link on pubmed article page"

-- getVenue :: [Node] -> Either String T.Text
getVenue nodes = maybe (Left "couldn't get venue") Right $ do
    let venueNodes = nodes ^.. traverse.allAttributed [("class", "article-source")]
    jname <- venueNodes ^? traverse.allAttributed [("id", "full-view-journal-trigger")].nodeToken._Tag.tagAttrs.ix "title"
    jcit <- venueNodes ^? traverse.allAttributed [("class", "cit")].allText
    date <- case T.splitOn ";" jcit of
        date: _ -> Just date
        _       -> Nothing
    return (jname, jcit, date)

getTitle :: [Node] -> Either String T.Text
getTitle nodes =
    case nodes ^? traverse.allAttributed [("class", "heading-title")].allText of
        Just t -> Right (clean t)
        Nothing -> Left "Couldn't locate title"  

getAuthors nodes = maybe (Left "Couldn't locate authors list") Right $ do
    authorList <- nodes ^? traverse.allAttributed [("class", "authors-list")]
    let authorNodes = authorList ^.. nodeChildren.traverse.nodeChildren.ix 0.allText
    return authorNodes

getArticleData pmid = scrap (getPMArticlePage pmid) $ \ns ->
    (,,,) <$> getJournalLink ns <*> getVenue ns <*> getTitle ns <*> getAuthors ns

getISSN journalPage = scrap (pget journalPage) $ \ns -> maybe (Left "Couldn't find ISSN") Right $ do
    table <- ns ^? traverse.allAttributed [("class", "nlmcat_dl")]
    let entries = table ^.. nodeChildren.to (head . tail . dropWhile (\n -> n ^. allText /= "ISSN:")).nodeChildren.traverse.nodeToken._Text
    return entries

data PubmedInfo = PMI
  { pmTitle :: T.Text
  , pmAuthors :: [T.Text]
  , pmVenue :: T.Text
  , pmCit :: T.Text
  , pmPubDate :: T.Text
  , pmISSNs :: [T.Text]
  , pmCount :: Int
  , ocCount :: Int
  } deriving (Show, Generic)

instance ToJSON PubmedInfo

getPubmedInfo :: PMID -> Handler PubmedInfo 
getPubmedInfo pmid = do
    cnt <- check "pubmed count" $ getPMCount pmid
    (lnk, (venueName, venueCit, pubDate), title, authors) <- check "article data" $ getArticleData pmid
    issn <- check "ISSN" $ getISSN lnk
    occnt <- pure 0 -- liftIO $ getOCCount pmid
    return $ PMI title authors venueName venueCit pubDate issn cnt occnt

    where check lbl act = do
            r <- liftIO act
            case r of
                Left e -> throwError (errorOut lbl e)
                Right a -> return a
            
          errorOut lbl e = err400 { errBody = LT.encodeUtf8 $
            "Error in '" <> lbl <> "': " <> LT.pack e
          }

-- * API server

type API = "pub" :> Capture "pmid" PMID :> Post '[JSON] PubmedInfo
      -- :<|> "citations" :> ReqBody '[JSON] PMIDs :> Post '[JSON] [Pub]
      :<|> Raw

server :: FilePath -> Server API
server fp = getPubmedInfo :<|> serveDirectoryFileServer fp
-- server fp = liftIO . getCitationCounts :<|> serveDirectoryFileServer fp

main :: IO ()
main = do
  args <- getArgs
  case args of
    [portStr, wwwPath] | Just port <- readMaybe portStr -> run port $ serve (Servant.Proxy :: Servant.Proxy API) (server wwwPath)
    _                                          -> error "please specify a port as argument"
