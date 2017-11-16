-- This is necessary to allow us to have the same field name in multiple records
{-# LANGUAGE DuplicateRecordFields #-}

module Schema where

import Evergreen

data User = User
  { id :: PrimaryInt
  , firstName :: String
  , lastName :: String
  , createdAt :: Datetime
  , updatedAt :: Datetime
  }

data Message = Message
  { id :: PrimaryInt
  , from :: User
  , to :: User
  , color :: Color
  , createdAt :: Datetime
  , updatedAt :: Datetime
  }

data Color = Color
  { id :: PrimaryInt
  , hex :: String
  , createdAt :: Datetime
  , updatedAt :: Datetime
  }
