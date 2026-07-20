{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MyLib.Reduce (operate, ReduceIn(..)) where

import MyLib.Store (StoreIn(..)) 

import Clash.Prelude
import Control.Monad (guard)

import MyLib (Node(..), NodeType(..))

data ReduceIn a = ReduceIn
  { node1 :: Node a
  , node2 :: Node a
  , aux1  :: Node a
  , aux2  :: Node a
  , valid :: Bool
  } deriving (Eq, Show, Generic, NFDataX)

computeNode1 :: (Num a) => ReduceIn a -> Node a
computeNode1 input =
  if tag input.aux1 == NUM
  then Node { tag = NUM, value = value input.node1 + value input.aux1 }
  else input.aux1

operate :: (Num a) => ReduceIn a
                   -> Bool
                   -> (Maybe (Node a, Node a), StoreIn a, Bool)
operate input _ = 
  ( guard input.valid >>
      Just ( computeNode1 input
           , if tag input.aux1 == NUM then input.aux2 else input.node2
           )
  , StoreIn
      { node1 = input.node1
      , node2 = input.aux2
      , addr = value input.node2
      , valid = input.valid && tag input.aux1 /= NUM
      }
  , True
  )

