{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module COVID19.Data (
    Region(..)
  , Data
  , fetchData
  , fetchCachedData
  , matchRegion
  , lookupData
  , shiftDay
  , unshiftDay
  , dataToSeries
  , dataUrl
  ) where

import           Conduit
import           Control.Exception
import           Control.Monad
import           Data.Bifunctor
import           Data.Binary.Instances    ()
import           Data.CSV.Conduit
import           Data.Conduit
import           Data.Foldable
import           Data.Map                 (Map)
import           Data.Maybe
import           Data.String
import           Data.Text                (Text)
import           Data.Time
import           Data.Vector              (Vector)
import           GHC.Generics
import           Network.HTTP.Conduit
import           Numeric.Natural
import           System.Directory
import           System.IO.Error
import           Text.Read
import qualified Data.Binary              as Bi
import qualified Data.ByteString          as BS
import qualified Data.Conduit.Combinators as C
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Vector              as V

dataUrl :: String
-- dataUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
dataUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

data Region = R { rCountry  :: !Text
                , rProvince :: !(Maybe Text)
                }
  deriving (Show, Eq, Ord, Generic)

instance Bi.Binary Region

type Data = Map Region (Map Day Natural)

-- | Days since January 22, 2020
shiftDay
    :: Day
    -> Integer
shiftDay d = d `diffDays` fromGregorian 2020 1 22

-- | Inverse of 'shiftDay'
unshiftDay
    :: Integer
    -> Day
unshiftDay = (`addDays` fromGregorian 2020 1 22)

lookupData
    :: Region
    -> Data
    -> Map Day Natural
lookupData rg = M.unionsWith (+)
              . toList
              . M.filterWithKey (\k _ -> matchRegion rg k)

dataToSeries
    :: Map Day Natural
    -> [(Double, Double)]
dataToSeries = map (bimap (fromIntegral . shiftDay) fromIntegral)
             . M.toList

matchRegion
    :: Region
    -> Region
    -> Bool
matchRegion (R c Nothing ) (R x _) = c == x
matchRegion (R c (Just p)) (R x y) = c == x && (Just p == y || matchState)
  where
    matchState = isJust $ do
      ab <- M.lookup p stateAbbrevs
      q  <- y
      guard $ ab `T.isSuffixOf` q

parseRow
    :: Map Text Text
    -> Maybe (Region, Map Day Natural)
parseRow rw = do
    rCountry  <- M.lookup "Country/Region" rw
    rProvince <- mfilter (not . T.null) . Just <$> M.lookup "Province/State" rw
    let dats = M.fromList $ do
          (k, v) <- M.toList rw
          t      <- parseTimeM True defaultTimeLocale "%-m/%-d/%y" (T.unpack k)
          c      <- maybeToList $ readMaybe (T.unpack v)
          pure (t, c)
    pure (R{..}, dats)

histConduitCSV
    :: (MonadThrow m, IsString s, CSV s (Row Text))
    => ConduitM s (Region, Map Day Natural) m ()
histConduitCSV = intoCSV defCSVSettings
              .| C.concatMap parseRow

sinkHistCSV
    :: (MonadThrow m, IsString s, CSV s (Row Text))
    => ConduitM s o m Data
sinkHistCSV = histConduitCSV
           .| C.foldMap (uncurry M.singleton)

fetchData
    :: String       -- ^ URL
    -> IO Data
fetchData u = do
    req <- parseRequest u
    mgr <- newManager tlsManagerSettings
    runResourceT $ do
      resp <- http req mgr
      runConduit $ responseBody resp
                .| sinkHistCSV

fetchCachedData
    :: String
    -> FilePath
    -> IO Data
fetchCachedData u fp = do
    mday <- tryJust (guard . isDoesNotExistError) $
        utctDay <$> getModificationTime fp
    cday <- utctDay <$> getCurrentTime
    when (Right cday > mday) $ do
      hist <- fetchData u
      runConduitRes $ C.sourceLazy (Bi.encode hist)
                   .| C.sinkFileCautious fp
    Bi.decodeFile fp

stateAbbrevs :: Map Text Text
stateAbbrevs = M.fromList
  [ ("Alabama", "AL")
  , ("Alaska", "AK")
  , ("Arizona", "AZ")
  , ("Arkansas", "AR")
  , ("California", "CA")
  , ("Colorado", "CO")
  , ("Connecticut", "CT")
  , ("Delaware", "DE")
  , ("Florida", "FL")
  , ("Georgia", "GA")
  , ("Hawaii", "HI")
  , ("Idaho", "ID")
  , ("Illinois", "IL")
  , ("Indiana", "IN")
  , ("Iowa", "IA")
  , ("Kansas", "KS")
  , ("Kentucky", "KY")
  , ("Louisiana", "LA")
  , ("Maine", "ME")
  , ("Maryland", "MD")
  , ("Massachusetts", "MA")
  , ("Michigan", "MI")
  , ("Minnesota", "MN")
  , ("Mississippi", "MS")
  , ("Missouri", "MO")
  , ("Montana", "MT")
  , ("Nebraska", "NE")
  , ("Nevada", "NV")
  , ("New Hampshire", "NH")
  , ("New Jersey", "NJ")
  , ("New Mexico", "NM")
  , ("New York", "NY")
  , ("North Carolina", "NC")
  , ("North Dakota", "ND")
  , ("Ohio", "OH")
  , ("Oklahoma", "OK")
  , ("Oregon", "OR")
  , ("Pennsylvania", "PA")
  , ("Rhode Island", "RI")
  , ("South Carolina", "SC")
  , ("South Dakota", "SD")
  , ("Tennessee", "TN")
  , ("Texas", "TX")
  , ("Utah", "UT")
  , ("Vermont", "VT")
  , ("Virginia", "VA")
  , ("Washington", "WA")
  , ("West Virginia", "WV")
  , ("Wisconsin", "WI")
  , ("Wyoming", "WY")
  , ("American Samoa", "AS")
  , ("District of Columbia", "DC")
  , ("Federated States of Micronesia", "FM")
  , ("Guam", "GU")
  , ("Marshall Islands", "MH")
  , ("Northern Mariana Islands", "MP")
  , ("Palau", "PW")
  , ("Puerto Rico", "PR")
  , ("Virgin Islands", "VI")
  ]
