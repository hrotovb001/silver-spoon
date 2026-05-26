{-# LANGUAGE DataKinds #-}

module MyLib (someFunc) where

import Clash.Prelude
import qualified Prelude as P

manipulateWire :: Unsigned 8 -> Unsigned 8
manipulateWire x = (complement x) `shiftL` 2

someFunc :: P.IO ()
someFunc = do
  let inputVal  = 5  :: Unsigned 8
      outputVal = manipulateWire inputVal
  
  P.putStrLn ("Input  Value (Dec): " P.++ P.show inputVal)
  P.putStrLn ("Output Value (Dec): " P.++ P.show outputVal)

