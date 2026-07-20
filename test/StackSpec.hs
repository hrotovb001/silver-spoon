{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module StackSpec (spec) where

import Test.Hspec
import Clash.Prelude
import MyLib.Buffer (stack)
import MyLib (Node(..), NodeType(..))
import Prelude as P

spec :: Spec
spec = do
  describe "Stack Buffer Component (No Bounds Check On Push)" $ do
    it "push three items and then pop three items" $ do
      let inputs :: [Maybe (Node Integer, Node Integer)]
          inputs =
            [ Just ( Node { tag = NUM, value = 1 } 
                   , Node { tag = OPP, value = 1 }
                   )
            , Just ( Node { tag = ERA, value = 0 }
                   , Node { tag = NUM, value = 2 }
                   )
            , Just ( Node { tag = NUM, value = 3 }
                   , Node { tag = OPP, value = 5 }
                   )
            , Just ( Node { tag = ERA, value = 0 }
                   , Node { tag = NUM, value = 8 }
                   )
            , Nothing
            , Nothing
            , Nothing
            , Nothing
            , Nothing
            ]
          ready :: [Bool]
          ready =
            [ False
            , False
            , False
            , False
            , True
            , False
            , True
            , True
            , True
            ]
          rData :: [(Node Integer, Node Integer)]
          rData =
            [ errorX("no read this cycle")
            , errorX("no read this cycle")
            , errorX("no read this cycle")
            , errorX("no read this cycle")
            , errorX("no read this cycle")
            , errorX("no read this cycle")
            , ( Node { tag = NUM, value = 3 }
              , Node { tag = OPP, value = 5 }
              )           
            , ( Node { tag = ERA, value = 0 }
              , Node { tag = NUM, value = 2 }
              )
            , ( Node { tag = NUM, value = 1 }
              , Node { tag = OPP, value = 1 }
              ) 
            ]
          (rAddr, wr, outputs) = P.unzip3 $ simulateN @System 9 stack $ P.zip3 rData inputs ready
          wAddr = [fmap fst x | x <- wr]
          wData = [fmap snd x | x <- wr]
          expectedRAddr :: [Index 5]
          expectedRAddr = [0, 1, 2, 3, 2, 2, 1, 0, 0]
          expectedWAddr :: [Maybe (Index 5)]
          expectedWAddr = [Just 0, Just 1, Just 2, Just 3, Nothing, Nothing, Nothing, Nothing, Nothing]
          expectedWData :: [Maybe (Node Integer, Node Integer)]
          expectedWData =
            [ Just ( Node { tag = NUM, value = 1 }
                   , Node { tag = OPP, value = 1 }
                   )
            , Just ( Node { tag = ERA, value = 0 }
                   , Node { tag = NUM, value = 2 }
                   )
            , Just ( Node { tag = NUM, value = 3 }
                   , Node { tag = OPP, value = 5 }
                   )
            , Just ( Node { tag = ERA, value = 0 }
                   , Node { tag = NUM, value = 8 }
                   )
            , Nothing
            , Nothing
            , Nothing
            , Nothing
            , Nothing
            ]
          expectedOutputs :: [Maybe (Node Integer, Node Integer)]
          expectedOutputs =
            [ Nothing
            , Nothing
            , Nothing
            , Nothing
            , Just ( Node { tag = ERA, value = 0 }
                   , Node { tag = NUM, value = 8 }
                   )
            , Nothing
            , Just ( Node { tag = NUM, value = 3 }
                   , Node { tag = OPP, value = 5 }
                   )
            , Just ( Node { tag = ERA, value = 0 }
                   , Node { tag = NUM, value = 2 }
                   )
            , Just ( Node { tag = NUM, value = 1 }
                   , Node { tag = OPP, value = 1 }
                   )
            ]
      rAddr `shouldBe` expectedRAddr
      wAddr `shouldBe` expectedWAddr
      wData `shouldBe` expectedWData
      outputs `shouldBe` expectedOutputs
    it "writing three items in place" $ do
      let inputs :: [Maybe (Node Integer, Node Integer)]
          inputs =
            [ Just ( Node { tag = NUM, value = 1 } 
                   , Node { tag = OPP, value = 1 }
                   )
            , Just ( Node { tag = ERA, value = 0 }
                   , Node { tag = NUM, value = 2 }
                   )
            , Just ( Node { tag = NUM, value = 3 }
                   , Node { tag = OPP, value = 5 }
                   )
            , Nothing
            ]
          ready :: [Bool]
          ready =
            [ True
            , True
            , True
            , True
            ]
          rData :: [(Node Integer, Node Integer)]
          rData =
            [ errorX("no read this cycle")
            , errorX("no read this cycle")
            , errorX("no read this cycle")
            , errorX("no read this cycle")
            ]
          (rAddr, wr, outputs) = P.unzip3 $ simulateN @System 4 stack $ P.zip3 rData inputs ready
          wAddr = [fmap fst x | x <- wr]
          wData = [fmap snd x | x <- wr]
          expectedRAddr :: [Index 3]
          expectedRAddr = [0, 0, 0, 0]
          expectedWAddr :: [Maybe (Index 3)]
          expectedWAddr = [Just 0, Just 0, Just 0, Nothing]
          expectedWData :: [Maybe (Node Integer, Node Integer)]
          expectedWData =
            [ Just ( Node { tag = NUM, value = 1 }
                   , Node { tag = OPP, value = 1 }
                   )
            , Just ( Node { tag = ERA, value = 0 }
                   , Node { tag = NUM, value = 2 }
                   )
            , Just ( Node { tag = NUM, value = 3 }
                   , Node { tag = OPP, value = 5 }
                   )
            , Nothing
            ]
          expectedOutputs :: [Maybe (Node Integer, Node Integer)]
          expectedOutputs =
            [ Nothing
            , Just ( Node { tag = NUM, value = 1 }
                   , Node { tag = OPP, value = 1 }
                   )
            , Just ( Node { tag = ERA, value = 0 }
                   , Node { tag = NUM, value = 2 }
                   )
            , Just ( Node { tag = NUM, value = 3 }
                   , Node { tag = OPP, value = 5 }
                   )
            ]
      rAddr `shouldBe` expectedRAddr
      wAddr `shouldBe` expectedWAddr
      wData `shouldBe` expectedWData
      outputs `shouldBe` expectedOutputs

