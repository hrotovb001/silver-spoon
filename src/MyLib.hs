{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module MyLib (Node(..), NodeType(..)) where

import Clash.Prelude

data NodeType = ERA | NUM | OPP
  deriving (Eq, Show, Generic, NFDataX)

data Node a = Node { tag :: NodeType, value :: a }
  deriving (Eq, Show, Generic, NFDataX)


