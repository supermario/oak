-- This is necessary to allow us to have the same field in multiple records
{-# LANGUAGE DuplicateRecordFields #-}

module Schema where

data User = User
  { id :: Int
  , name :: String
  }

data Account = Account
  { id :: Int
  , userId :: Int
  }
