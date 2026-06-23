{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module MyLib.Buffer (stack, BufferIn(..), BufferOut(..)) where

import Control.Monad (guard)
import Clash.Prelude

data BufferIn a = BufferIn
  { payload :: Maybe a
  , ready :: Bool
  } deriving (Eq, Show, Generic, NFDataX)

data BufferOut a = BufferOut
  { payload :: Maybe a
  } deriving (Eq, Show, Generic, NFDataX)

data StackState a b = StackState
  { top :: Maybe a
  , sPtr :: b
  } deriving (Eq, Show, Generic, NFDataX)

stackT :: (SaturatingNum b, Eq b)
       => StackState a b
       -> (a, BufferIn a)
       -> (StackState a b, (b, Maybe (b, a), BufferOut a))
stackT state (rData, input@(BufferIn{ payload = Just p, ready = True })) =
  ( StackState
      { top = input.payload
      , sPtr = state.sPtr
      }
  , ( state.sPtr
    , Just (state.sPtr, p)
    , BufferOut { payload = state.top <|> (guard (state.sPtr /= 0) >> Just rData) }
    )
  )
stackT state (_, input@(BufferIn{ payload = Just p, ready = False })) =
  ( StackState
      { top = input.payload
      , sPtr = state.sPtr + 1
      }
  , ( state.sPtr
    , Just (state.sPtr, p)
    , BufferOut { payload = Nothing }
    )
  )
stackT state (rData, BufferIn{ payload = Nothing, ready = True }) =
  ( StackState
      { top = Nothing
      , sPtr = satSub SatZero state.sPtr 1
      }
  , ( satSub SatZero state.sPtr 2
    , Nothing
    , BufferOut { payload = state.top <|> (guard (state.sPtr /= 0) >> Just rData) }
    )
  )
stackT state (_, BufferIn{ payload = Nothing, ready = False }) =
  ( state
  , ( satSub SatZero state.sPtr 1
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
                  , sPtr = 0
                  }
