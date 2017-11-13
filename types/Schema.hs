-- This is necessary to allow us to have the same field name in multiple records
{-# LANGUAGE DuplicateRecordFields #-}

module Schema where

import Evergreen

data User = User
  { id :: SerialPrimaryKey
  , firstName :: String
  , lastName :: String
  , account :: Account
  }

data Account = Account
  { id :: SerialPrimaryKey
  , userId :: Maybe Int
  , user :: User
  }
