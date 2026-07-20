{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module StoreSpec (spec) where

import Test.Hspec
import MyLib.Store (store, StoreIn(..))
import MyLib (Node(..), NodeType(..))

spec :: Spec
spec = do
  describe "Node Store Component" $ do
    it "add new redex to stack after reduction" $ do
      let input :: StoreIn Integer
          input =
            StoreIn
              { node1 = Node { tag = ERA, value = 0 }
              , node2 = Node { tag = ERA, value = 0 }
              , addr = 0
              , valid = False
              }
          wr = store input
      wr `shouldBe` Nothing
    it "update aux nodes in memory after reduction" $ do
      let input :: StoreIn Integer
          input =
            StoreIn
              { node1 = Node { tag = NUM, value = 8 }
              , node2 = Node { tag = OPP, value = 9 }
              , addr = 5
              , valid = True
              }
          wr = store input
          (wAddr, wData) = (fmap fst wr, fmap snd wr)
          expectedWAddr :: Maybe Integer
          expectedWAddr = Just 5
          expectedWData :: Maybe (Node Integer, Node Integer)
          expectedWData = Just ( Node { tag = NUM, value = 8 }
                               , Node { tag = OPP, value = 9 }
                               )
      wAddr `shouldBe` expectedWAddr
      wData `shouldBe` expectedWData
