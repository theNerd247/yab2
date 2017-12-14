{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Budget.StartInfo where

import Control.Applicative ((<**>))
import Control.Lens hiding ((.=), Indexable)
import Data.Aeson
import Data.BID
import Data.Data
import GHC.Generics
import Data.Budget.Amount
import Data.Budget.Name
import Data.Default
import Data.Default.Time
import Data.Budget.Items
import Data.IxSet
import Data.SafeCopy
import Data.Time

data StartInfo = StartInfo
  { _startInfoBName :: Name
  , _startDate :: UTCTime
  , _startAmount :: Amount
  , _startInfoBID :: BID
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

makeClassy ''StartInfo

$(deriveSafeCopy 0 'base ''StartInfo)

instance Default StartInfo

instance Indexable StartInfo where
  empty = ixSet 
    [ ixFun $ (:[]) . (view bid) 
    , ixFun $ (:[]) . (view name)
    , ixFun $ (:[]) . (view startDate)
    , ixFun $ (:[]) . (view startAmount)
    ]

instance HasName StartInfo where
  name = startInfoBName

instance HasBID StartInfo where
  bid = startInfoBID

instance FromJSON StartInfo where
  parseJSON v@(Object o) = do
    sd <- o .: "startDate"
    sa <- o .: "startAmount"
    pure (def & startDate .~ sd & startAmount .~ sa) <**> parseNameJSON v <**> parseBIDJSON v
  parseJSON _ = mempty

instance ToJSON StartInfo where
  toJSON b = object $ 
       (toNameJSON b)
    ++ (toBIDJSON b)
    ++ ["startDate" .= (b^.startDate)
       ,"startAmount" .= (b^.startAmount)
       ]

instance HasDate StartInfo where
  date = startDate

toStartInfoJSON b = ["startInfo" .= (b^.startInfo)]

parseStartInfoJSON (Object o) = do
  si <- o .: "startInfo"
  return $ \v -> v & startInfo .~ si
