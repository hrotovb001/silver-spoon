{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}

module MyLib.Buffer (stack, BufferIn(..), BufferOut(..)) where

import Clash.Prelude
import Data.Maybe (isJust)

data BufferIn a = BufferIn
  { payload :: Maybe a
  , ready :: Bool
  } deriving (Eq, Show, Generic, NFDataX)

data BufferOut a = BufferOut
  { payload :: Maybe a
  } deriving (Eq, Show, Generic, NFDataX)

data StackState a b = StackState
  { top :: Maybe a
  , next :: Maybe a
  , sPtr :: b
  } deriving (Eq, Show, Generic, NFDataX)

stackT :: (SaturatingNum b, Eq b)
       => StackState a b
       -> (a, BufferIn a)
       -> (StackState a b, (b, Maybe (b, a), BufferOut a))
stackT state (_, input@(BufferIn{ payload = Just _, ready = True })) =
  ( StackState
      { top = input.payload
      , next = state.next
      , sPtr = state.sPtr
      }
  , ( state.sPtr
    , Nothing
    , BufferOut { payload = state.top }
    )
  )
stackT state (_, input@(BufferIn{ payload = Just _, ready = False })) =
  ( StackState
      { top = input.payload
      , next = state.top
      , sPtr = if isJust state.top then state.sPtr + 1 else state.sPtr
      }
  , ( state.sPtr
    , (state.sPtr, ) <$> state.top
    , BufferOut { payload = Nothing }
    )
  )
stackT state (rData, BufferIn{ payload = Nothing, ready = True }) =
  ( StackState
      { top = if isJust state.next then state.next else Just rData
      , next = Nothing
      , sPtr = satSub SatZero state.sPtr 1
      }
  , ( satSub SatZero state.sPtr 2
    , Nothing
    , BufferOut { payload = state.top }
    )
  )
stackT state (_, BufferIn{ payload = Nothing, ready = False }) =
  ( state
  , ( state.sPtr
    , Nothing
    , BufferOut { payload = Nothing }
    )
  )

stack ::
  ( NFDataX a
  , NFDataX b
  , SaturatingNum b
  , Num b
  , Eq b
  , KnownDomain dom
  , HiddenClockResetEnable dom
  )
  => Signal dom (a, BufferIn a)
  -> Signal dom (b, Maybe (b, a), BufferOut a)
stack = mealy stackT state
  where state = StackState
                  { top = Nothing
                  , next = Nothing
                  , sPtr = 0
                  }
