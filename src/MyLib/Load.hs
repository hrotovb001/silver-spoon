{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module MyLib.Load (load, LoadIn(..), LoadOut(..)) where

import MyLib (Node(..), NodeType(..))
import Clash.Prelude
import qualified Clash.Prelude as P

data LoadIn a = LoadIn
  { node1 :: Node a
  , node2 :: Node a
  , valid :: Bool
  , ready :: Bool
  } deriving (Eq, Show, Generic, NFDataX)

data LoadOut a = LoadOut
  { node1 :: Node a
  , node2 :: Node a
  , aux1 :: Node a
  , aux2 :: Node a
  , valid :: Bool
  } deriving (Eq, Show, Generic, NFDataX)

data LoadState a = LoadState
  { node1 :: Node a
  , node2 :: Node a
  , valid :: Bool
  } deriving (Eq, Show, Generic, NFDataX)

nextState :: LoadState a -> LoadIn a -> LoadState a
nextState _ input@(LoadIn{ ready = True }) =
  LoadState
    { node1 = input.node1
    , node2 = input.node2
    , valid = input.valid
    }
nextState state LoadIn{ ready = False } = state

loadT :: Num a
      => LoadState a
      -> ((Node a, Node a), LoadIn a)
      -> (LoadState a, (a, LoadOut a))
loadT state (rData, input) =
  ( nextState state input
  , ( if input.valid then value input.node2 else value state.node2
    , LoadOut
        { node1 = state.node1
        , node2 = state.node2
        , aux1 = fst rData
        , aux2 = snd rData
        , valid = input.ready P.&& state.valid
        }
    )
  )

load ::
  ( NFDataX a
  , Num a
  , KnownDomain dom
  , HiddenClockResetEnable dom
  )
  => Signal dom ((Node a, Node a), LoadIn a)
  -> Signal dom (a, LoadOut a)
load = mealy loadT state
  where state = LoadState
                  { node1 = Node { tag = ERA, value = 0 }
                  , node2 = Node { tag = ERA, value = 0 }
                  , valid = False
                  }
