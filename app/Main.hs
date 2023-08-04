{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-
 - Log failed http calls
 - Progress bar
 - Remember/Star button
 -}
module Main where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString as BSL
import Control.Applicative ((<|>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (find)
import Data.String (fromString)
import GHC.Generics (Generic)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.HTML.Scalpel

data Listing = Listing
    { title :: String
    , description :: String
    , price :: String
    , link :: String
    , image :: String
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Search = Search
  { url :: String
  , listings :: [Listing]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

loadLinks :: IO [String]
loadLinks = lines <$> readFile "links.txt"

scrapeSearch :: String -> IO (Maybe Search)
scrapeSearch url = scrapeURL url searchS
  where
    searchS :: Scraper String Search
    searchS = Search url <$> listingsS

    listingsS :: Scraper String [Listing]
    listingsS = chroots ("article" @: [hasClass "aditem"]) listingS

    listingS :: Scraper String Listing
    listingS = do
      title <- text $ "h2" // "a"
      description <- text $ "p" @: [hasClass "aditem-main--middle--description"]
      price <- text $ "p" @: [hasClass "aditem-main--middle--price-shipping--price"]
      link <- attr "href" $ "h2" // "a"
      -- TODO: Default image
      image <- attr "src" "img" <|> pure ""
      return $ Listing title description price link image

filterNewArticles :: [Search] -> [Search] -> [Search]
filterNewArticles newSearch oldSearch = filterSearch <$> newSearch
  where
    filterSearch (Search url listings) =
      case findOldSearch url of
        Just (Search _ oldListings) -> Search url (filter (`notElem` oldListings) listings)
        Nothing -> Search url listings
    findOldSearch l = find ((== l) . url) oldSearch

readSearches :: IO [Search]
readSearches = do
  content <- BS.readFile "searches"
  return (fromMaybe [] $ decode $ BSL.fromStrict content)

writeSearches :: [Search] -> IO ()
writeSearches searches = BS.writeFile "searches" $ BSL.toStrict $ encode searches

renderResult :: [Search] -> H.Html
renderResult searches = H.html $ do
  H.head $ H.title "Angebote"
  H.body $ do
    H.h1 "Angebote"
    mconcat (renderSearch <$> searches)

renderSearch :: Search -> H.Html
renderSearch (Search url listings) = do
  (H.a $ H.h2 $ H.toHtml url) ! A.href (fromString url)
  mconcat (renderListing <$> listings)

renderListing :: Listing -> H.Html
renderListing Listing {..} = do
  H.h3 $ (((H.span $ H.toHtml price) ! A.style "color: red") <> H.toHtml title)
  (H.div $ do
    H.a (H.img ! A.src (fromString image)) ! A.href (fromString ("https://www.kleinanzeigen.de" ++ link)) ! A.style "margin-right: 10px;"
    H.p $ H.toHtml description) ! A.style "display: flex"

main :: IO ()
main = do
  links <- loadLinks
  newSearches <- catMaybes <$> (scrapeSearch `mapM` links)
  oldSearches <- readSearches
  writeSearches newSearches
  writeFile "./static/index.html" $ renderHtml $ renderResult $ filterNewArticles newSearches oldSearches


