{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module MyLib.Buffer (stack) where

import Control.Monad (guard)
import Clash.Prelude

data StackState a b = StackState
  { top :: Maybe a
  , sPtr :: b
  } deriving (Eq, Show, Generic, NFDataX)

stackT :: (SaturatingNum b, Eq b)
       => StackState a b
       -> (a, Maybe a, Bool)
       -> (StackState a b, (b, Maybe (b, a), Maybe a))
stackT state (rData, Just p, True) =
  ( StackState
      { top = Just p
      , sPtr = state.sPtr
      }
  , ( state.sPtr
    , Just (state.sPtr, p)
    , state.top <|> (guard (state.sPtr /= 0) >> Just rData)
    )
  )
stackT state (_, Just p, False) =
  ( StackState
      { top = Just p
      , sPtr = state.sPtr + 1
      }
  , ( state.sPtr
    , Just (state.sPtr, p)
    , Nothing
    )
  )
stackT state (rData, Nothing, True) =
  ( StackState
      { top = Nothing
      , sPtr = satSub SatZero state.sPtr 1
      }
  , ( satSub SatZero state.sPtr 2
    , Nothing
    , state.top <|> (guard (state.sPtr /= 0) >> Just rData)
    )
  )
stackT state (_, Nothing, False) =
  ( state
  , ( satSub SatZero state.sPtr 1
    , Nothing
    , Nothing
    )
  )

stack ::
  ( NFDataX a
  , NFDataX b
  , SaturatingNum b
  , Enum b
  , Eq b
  , KnownDomain dom
  , HiddenClockResetEnable dom
  )
  => Signal dom (a, Maybe a, Bool)
  -> Signal dom (b, Maybe (b, a), Maybe a)
stack = mealy stackT state
  where state = StackState
                  { top = Nothing
                  , sPtr = 0
                  }
