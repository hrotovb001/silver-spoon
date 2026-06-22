{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module StackSpec (spec) where

import Test.Hspec
import Clash.Prelude
import MyLib.Buffer (stack, BufferIn(..), BufferOut(..))
import MyLib (Node(..), NodeType(..))
import Prelude as P

spec :: Spec
spec = do
  describe "Stack Buffer Component (No Bounds Check On Push)" $ do
    it "push three items and then pop three items" $ do
      let inputs :: [BufferIn (Node Integer, Node Integer)]
          inputs =
            [ BufferIn
                { payload = Just ( Node { tag = NUM, value = 1 } 
                                 , Node { tag = OPP, value = 1 }
                                 )
                , ready = False
                }
            , BufferIn
                { payload = Just ( Node { tag = ERA, value = 0 }
                                 , Node { tag = NUM, value = 2 }
                                 )
                , ready = False
                }
            , BufferIn
                { payload = Just ( Node { tag = NUM, value = 3 }
                                 , Node { tag = OPP, value = 5 }
                                 )
                , ready = False
                }
            , BufferIn
                { payload = Just ( Node { tag = ERA, value = 0 }
                                 , Node { tag = NUM, value = 8 }
                                 )
                , ready = False
                }
            , BufferIn
                { payload = Nothing
                , ready = True
                }
            , BufferIn
                { payload = Nothing
                , ready = True
                }
            , BufferIn
                { payload = Nothing
                , ready = True
                }
            , BufferIn
                { payload = Nothing
                , ready = True
                }
            ]
          rData :: [(Node Integer, Node Integer)]
          rData =
            [ errorX("no read this cycle")
            , errorX("no read this cycle")
            , errorX("no read this cycle")
            , errorX("no read this cycle")
            , errorX("no read this cycle")
            , ( Node { tag = ERA, value = 0 }
              , Node { tag = NUM, value = 2 }
              )
            , ( Node { tag = NUM, value = 1 }
              , Node { tag = OPP, value = 1 }
              ) 
            , errorX("no read this cycle")
            ]
          (rAddr, wr, outputs) = P.unzip3 $ simulateN @System 8 stack $ P.zip rData inputs 
          wAddr = [fmap fst x | x <- wr]
          wData = [fmap snd x | x <- wr]
          expectedRAddr :: [Index 4]
          expectedRAddr = [0, 0, 1, 2, 1, 0, 0, 0]
          expectedWAddr :: [Maybe (Index 4)]
          expectedWAddr = [Nothing, Just 0, Just 1, Just 2, Nothing, Nothing, Nothing, Nothing]
          expectedWData :: [Maybe (Node Integer, Node Integer)]
          expectedWData =
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
            , Nothing
            , Nothing
            , Nothing
            , Nothing
            ]
          expectedOutputs :: [BufferOut (Node Integer, Node Integer)]
          expectedOutputs =
            [ BufferOut { payload = Nothing }
            , BufferOut { payload = Nothing }
            , BufferOut { payload = Nothing }
            , BufferOut { payload = Nothing }
            , BufferOut
                { payload = Just ( Node { tag = ERA, value = 0 }
                                 , Node { tag = NUM, value = 8 }
                                 )
                }
            , BufferOut
                { payload = Just ( Node { tag = NUM, value = 3 }
                                 , Node { tag = OPP, value = 5 }
                                 )
                }
            , BufferOut
                { payload = Just ( Node { tag = ERA, value = 0 }
                                 , Node { tag = NUM, value = 2 }
                                 )
                }
            , BufferOut
                { payload = Just ( Node { tag = NUM, value = 1 }
                                 , Node { tag = OPP, value = 1 }
                                 )
                }
            ]
      rAddr `shouldBe` expectedRAddr
      wAddr `shouldBe` expectedWAddr
      wData `shouldBe` expectedWData
      outputs `shouldBe` expectedOutputs
    it "writing three items in place" $ do
      let inputs :: [BufferIn (Node Integer, Node Integer)]
          inputs =
            [ BufferIn
                { payload = Just ( Node { tag = NUM, value = 1 } 
                                 , Node { tag = OPP, value = 1 }
                                 )
                , ready = True
                }
            , BufferIn
                { payload = Just ( Node { tag = ERA, value = 0 }
                                 , Node { tag = NUM, value = 2 }
                                 )
                , ready = True
                }
            , BufferIn
                { payload = Just ( Node { tag = NUM, value = 3 }
                                 , Node { tag = OPP, value = 5 }
                                 )
                , ready = True
                }
            , BufferIn
                { payload = Nothing
                , ready = True
                }
            ]
          rData :: [(Node Integer, Node Integer)]
          rData =
            [ errorX("no read this cycle")
            , errorX("no read this cycle")
            , errorX("no read this cycle")
            , errorX("no read this cycle")
            ]
          (rAddr, wr, outputs) = P.unzip3 $ simulateN @System 4 stack $ P.zip rData inputs 
          wAddr = [fmap fst x | x <- wr]
          wData = [fmap snd x | x <- wr]
          expectedRAddr :: [Index 3]
          expectedRAddr = [0, 0, 0, 0]
          expectedWAddr :: [Maybe (Index 3)]
          expectedWAddr = [Nothing, Nothing, Nothing, Nothing]
          expectedWData :: [Maybe (Node Integer, Node Integer)]
          expectedWData =
            [ Nothing
            , Nothing
            , Nothing
            , Nothing
            ]
          expectedOutputs :: [BufferOut (Node Integer, Node Integer)]
          expectedOutputs =
            [ BufferOut { payload = Nothing }
            , BufferOut
                { payload = Just ( Node { tag = NUM, value = 1 }
                                 , Node { tag = OPP, value = 1 }
                                 )
                }
            , BufferOut
                { payload = Just ( Node { tag = ERA, value = 0 }
                                 , Node { tag = NUM, value = 2 }
                                 )
                }
            , BufferOut
                { payload = Just ( Node { tag = NUM, value = 3 }
                                 , Node { tag = OPP, value = 5 }
                                 )
                }
            ]
      rAddr `shouldBe` expectedRAddr
      wAddr `shouldBe` expectedWAddr
      wData `shouldBe` expectedWData
      outputs `shouldBe` expectedOutputs

