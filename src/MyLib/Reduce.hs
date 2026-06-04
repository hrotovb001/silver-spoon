{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module MyLib.Reduce (operate, PinsIn(..), PinsOut(..)) where

import Clash.Prelude

import MyLib (Node(..), NodeType(..))

data PinsIn a = PinsIn
  { node1 :: Node a
  , node2 :: Node a
  , aux1  :: Node a
  , aux2  :: Node a
  } deriving (Eq, Show, Generic, NFDataX)

data PinsOut a = PinsOut
  { node1 :: Node a
  , node2 :: Node a
  , push  :: Bool
  , addr  :: a
  , aux1  :: Node a
  , aux2  :: Node a
  , write :: Bool
  , ready  :: Bool
  } deriving (Eq, Show, Generic, NFDataX)

operate :: (Num a) => PinsIn a -> PinsOut a
operate input@(PinsIn{ aux1 = Node{ tag = NUM } }) = 
  PinsOut
    { node1 = Node { tag = NUM, value = value input.node1 + value input.aux1 }
    , node2 = input.aux2
    , push = True
    , addr = value input.node2
    , aux1 = Node { tag = ERA, value = 0 }
    , aux2 = Node { tag = ERA, value = 0 }
    , write = False
    , ready = True
    }

operate input = 
  PinsOut
    { node1 = input.aux1
    , node2 = input.node2
    , push = True
    , addr = value input.node2
    , aux1 = input.node1
    , aux2 = input.aux2
    , write = True
    , ready = True
    }
  
