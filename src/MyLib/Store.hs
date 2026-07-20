{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module MyLib.Store (store, StoreIn(..)) where

import Clash.Prelude
import MyLib (Node(..))

data StoreIn a = StoreIn
  { node1 :: Node a
  , node2 :: Node a
  , addr :: a
  , valid :: Bool
  } deriving (Eq, Show, Generic, NFDataX)

store :: StoreIn a -> Maybe (a, (Node a, Node a))
store input =
  if input.valid
  then Just
         ( input.addr
         , ( input.node1
           , input.node2
           )
         )
  else Nothing
