{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module MyLib.Reduce (operate, ReduceIn(..), ReduceOut(..)) where

import Clash.Prelude

import MyLib (Node(..), NodeType(..))

data ReduceIn a = ReduceIn
  { node1 :: Node a
  , node2 :: Node a
  , aux1  :: Node a
  , aux2  :: Node a
  } deriving (Eq, Show, Generic, NFDataX)

data ReduceOut a = ReduceOut
  { node1 :: Node a
  , node2 :: Node a
  , push  :: Bool
  , addr  :: a
  , aux1  :: Node a
  , aux2  :: Node a
  , write :: Bool
  , ready  :: Bool
  } deriving (Eq, Show, Generic, NFDataX)

operate :: (Num a) => ReduceIn a -> ReduceOut a
operate input@(ReduceIn{ aux1 = Node{ tag = NUM } }) = 
  ReduceOut
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
  ReduceOut
    { node1 = input.aux1
    , node2 = input.node2
    , push = True
    , addr = value input.node2
    , aux1 = input.node1
    , aux2 = input.aux2
    , write = True
    , ready = True
    }
  
