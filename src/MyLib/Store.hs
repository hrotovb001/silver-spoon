{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module MyLib.Store (store, StoreIn(..), StoreOut(..)) where

import Clash.Prelude
import MyLib (Node(..))

data StoreIn a = StoreIn
  { node1 :: Node a
  , node2 :: Node a
  , push :: Bool
  , addr :: a
  , aux1 :: Node a
  , aux2 :: Node a
  , write :: Bool
  } deriving (Eq, Show, Generic, NFDataX)

data StoreOut a = StoreOut
  { node1 :: Node a
  , node2 :: Node a
  , valid :: Bool
  } deriving (Eq, Show, Generic, NFDataX)

store :: StoreIn a -> (Maybe (a, (Node a, Node a)), StoreOut a)
store input =
  ( if input.write
    then Just
           ( input.addr
           , ( input.aux1
             , input.aux2
             )
           )
    else Nothing
  , StoreOut
      { node1 = input.node1
      , node2 = input.node2
      , valid = input.push
      }
  )
