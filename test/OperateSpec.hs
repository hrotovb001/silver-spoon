{-# LANGUAGE DuplicateRecordFields #-}

module OperateSpec (spec) where

import Test.Hspec
import MyLib.Reduce (operate, ReduceIn(..))
import MyLib.Store (StoreIn(..))
import MyLib (Node(..), NodeType(..))

spec :: Spec
spec = do
  describe "Operation Reducer" $ do
    it "aux1 is a number" $ do
      let input :: ReduceIn Integer
          input = ReduceIn
                    { node1 = Node { tag = NUM, value = 10 } 
                    , node2 = Node { tag = OPP, value = 1 }
                    , aux1 = Node { tag = NUM, value = 5 }
                    , aux2 = Node { tag = OPP, value = 26 }
                    , valid = True
                    }
          (bufferData, storeData, ready) = operate input True
          expectedBufferData :: Maybe (Node Integer, Node Integer)
          expectedBufferData = Just ( Node { tag = NUM, value = 15 }
                                    , Node { tag = OPP, value = 26 }
                                    )
          expectedStoreData :: StoreIn Integer
          expectedStoreData = StoreIn
                                { node1 = Node { tag = NUM, value = 10 }
                                , node2 = Node { tag = OPP, value = 26 }
                                , addr = 1
                                , valid = False
                                }
      bufferData `shouldBe` expectedBufferData
      storeData `shouldBe` expectedStoreData
      ready `shouldBe` True
    it "aux1 is not a number" $ do
      let input :: ReduceIn Integer
          input = ReduceIn
                    { node1 = Node { tag = NUM, value = 10 } 
                    , node2 = Node { tag = OPP, value = 2 }
                    , aux1 = Node { tag = OPP, value = 21 }
                    , aux2 = Node { tag = OPP, value = 42 }
                    , valid = True
                    }
          (bufferData, storeData, ready) = operate input True
          expectedBufferData :: Maybe (Node Integer, Node Integer)
          expectedBufferData = Just ( Node { tag = OPP, value = 21 }
                                    , Node { tag = OPP, value = 2 }
                                    )
          expectedStoreData :: StoreIn Integer
          expectedStoreData = StoreIn
                                { node1 = Node { tag = NUM, value = 10 }
                                , node2 = Node { tag = OPP, value = 42 }
                                , addr = 2
                                , valid = True
                                }
      bufferData `shouldBe` expectedBufferData
      storeData `shouldBe` expectedStoreData
      ready `shouldBe` True
    it "input invalid" $ do
      let input :: ReduceIn Integer
          input = ReduceIn
                    { node1 = Node { tag = NUM, value = 10 } 
                    , node2 = Node { tag = OPP, value = 2 }
                    , aux1 = Node { tag = OPP, value = 21 }
                    , aux2 = Node { tag = OPP, value = 42 }
                    , valid = False
                    }
          (bufferData, storeData, ready) = operate input True
          expectedBufferData :: Maybe (Node Integer, Node Integer)
          expectedBufferData = Nothing
          expectedStoreData :: StoreIn Integer
          expectedStoreData = StoreIn  
                                { node1 = Node { tag = NUM, value = 10 }
                                , node2 = Node { tag = OPP, value = 42 }
                                , addr = 2
                                , valid = False
                                }
      bufferData `shouldBe` expectedBufferData
      storeData `shouldBe` expectedStoreData
      ready `shouldBe` True
