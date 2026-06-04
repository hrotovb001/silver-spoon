{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module OperateSpec (spec) where

import Test.Hspec
import MyLib.Reduce (operate, PinsIn(..), PinsOut(..))
import MyLib (Node(..), NodeType(..))

spec :: Spec
spec = do
  describe "Operation Reducer" $ do
    it "aux1 is a number" $ do
      let input :: PinsIn Integer
          input = PinsIn
                     { node1 = Node { tag = NUM, value = 10 } 
                     , node2 = Node { tag = OPP, value = 1 }
                     , aux1 = Node { tag = NUM, value = 5 }
                     , aux2 = Node { tag = OPP, value = 26 }
                     }
          output = operate input
          expected :: PinsOut Integer
          expected = PinsOut
                       { node1 = Node { tag = NUM, value = 15 }
                       , node2 = Node { tag = OPP, value = 26 }
                       , push = True
                       , addr = 1
                       , aux1 = Node { tag = ERA, value = 0 }
                       , aux2 = Node { tag = ERA, value = 0 }
                       , write = False
                       , ready = True
                       }
      output `shouldBe` expected

    it "aux1 is not a number" $ do
      let input :: PinsIn Integer
          input = PinsIn
                     { node1 = Node { tag = NUM, value = 10 } 
                     , node2 = Node { tag = OPP, value = 2 }
                     , aux1 = Node { tag = OPP, value = 21 }
                     , aux2 = Node { tag = OPP, value = 42 }
                     }
          output = operate input
          expected :: PinsOut Integer
          expected = PinsOut
                       { node1 = Node { tag = OPP, value = 21 }
                       , node2 = Node { tag = OPP, value = 2 }
                       , push = True
                       , addr = 2
                       , aux1 = Node { tag = NUM, value = 10 }
                       , aux2 = Node { tag = OPP, value = 42 }
                       , write = True
                       , ready = True
                       }
      output `shouldBe` expected

