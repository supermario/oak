-- This is necessary to allow us to have the same field name in multiple records
{-# LANGUAGE DuplicateRecordFields #-}

module Schema where

import Evergreen

data ColorX = ColorX
  { id :: PrimaryInt
  , hex :: String
  , user :: User
  , createdAt :: Datetime
  , updatedAt :: Datetime
  }

data Message = Message
  { id :: PrimaryInt
  , user :: User
  , createdAt :: Datetime
  , updatedAt :: Datetime
  }

data User = User
  { id :: PrimaryInt
  , firstName :: String
  , lastName :: String
  , color :: ColorX
  , createdAt :: Datetime
  , updatedAt :: Datetime
  }
