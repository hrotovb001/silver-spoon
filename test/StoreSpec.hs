{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module StoreSpec (spec) where

import Test.Hspec
import MyLib.Store (store, StoreIn(..), StoreOut(..))
import MyLib (Node(..), NodeType(..))

spec :: Spec
spec = do
  describe "Node Store Component" $ do
    it "add new redex to stack after reduction" $ do
      let input :: StoreIn Integer
          input =
            StoreIn
              { node1 = Node { tag = NUM, value = 6 }
              , node2 = Node { tag = OPP, value = 7 }
              , push = True
              , addr = 0
              , aux1 = Node { tag = ERA, value = 0 }
              , aux2 = Node { tag = ERA, value = 0 }
              , write = False
              }
          (wr, output) = store input
          expectedOutput =
            StoreOut
              { node1 = Node { tag = NUM, value = 6 }
              , node2 = Node { tag = OPP, value = 7 }
              , valid = True
              }
      wr `shouldBe` Nothing
      output `shouldBe` expectedOutput
    it "update aux nodes in memory after reduction" $ do
      let input :: StoreIn Integer
          input =
            StoreIn
              { node1 = Node { tag = ERA, value = 0 }
              , node2 = Node { tag = ERA, value = 0 }
              , push = False
              , addr = 5
              , aux1 = Node { tag = NUM, value = 8 }
              , aux2 = Node { tag = OPP, value = 9 }
              , write = True
              }
          (wr, output) = store input
          (wAddr, wData) = (fmap fst wr, fmap snd wr)
          expectedWAddr :: Maybe Integer
          expectedWAddr = Just 5
          expectedWData :: Maybe (Node Integer, Node Integer)
          expectedWData = Just ( Node { tag = NUM, value = 8 }
                               , Node { tag = OPP, value = 9 }
                               )
          expectedOutput =
            StoreOut
              { node1 = Node { tag = ERA, value = 0 }
              , node2 = Node { tag = ERA, value = 0 }
              , valid = False
              }
      wAddr `shouldBe` expectedWAddr
      wData `shouldBe` expectedWData
      output `shouldBe` expectedOutput
