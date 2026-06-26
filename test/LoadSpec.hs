{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module LoadSpec (spec) where

import Test.Hspec
import Clash.Prelude
import MyLib.Load (load, LoadIn(..), LoadOut(..))
import MyLib (Node(..), NodeType(..))
import Prelude as P

spec :: Spec
spec = do
  describe "Auxillary Data Loader Component" $ do
    it "load in aux nodes of operation node" $ do
      let inputs :: [LoadIn Integer]
          inputs =
            [ LoadIn
                { node1 = Node { tag = NUM, value = 1 }
                , node2 = Node { tag = OPP, value = 2 }
                , valid = True
                , ready = True
                }
            , LoadIn
                { node1 = Node { tag = NUM, value = 3 }
                , node2 = Node { tag = OPP, value = 5 }
                , valid = True
                , ready = True
                }
            , LoadIn
                { node1 = Node { tag = ERA, value = 0 }
                , node2 = Node { tag = ERA, value = 0 }
                , valid = False
                , ready = True
                }
            , LoadIn
                { node1 = Node { tag = NUM, value = 8 }
                , node2 = Node { tag = OPP, value = 13 }
                , valid = True
                , ready = True
                }
            , LoadIn
                { node1 = Node { tag = NUM, value = 21 }
                , node2 = Node { tag = OPP, value = 34 }
                , valid = True
                , ready = False
                }
            , LoadIn
                { node1 = Node { tag = NUM, value = 21 }
                , node2 = Node { tag = OPP, value = 34 }
                , valid = True
                , ready = True
                }
            , LoadIn
                { node1 = Node { tag = ERA, value = 0 }
                , node2 = Node { tag = ERA, value = 0 }
                , valid = False
                , ready = True
                }
            , LoadIn
                { node1 = Node { tag = ERA, value = 0 }
                , node2 = Node { tag = ERA, value = 0 }
                , valid = False
                , ready = False
                }
            ]
          rData :: [(Node Integer, Node Integer)]
          rData =
            [ ( Node { tag = ERA, value = 0 }
              , Node { tag = ERA, value = 0 }
              )
            , ( Node { tag = NUM, value = 7 }
              , Node { tag = OPP, value = 4 }
              )
            , ( Node { tag = NUM, value = 11 }
              , Node { tag = OPP, value = 6 }
              )
            , ( Node { tag = NUM, value = 11 }
              , Node { tag = OPP, value = 6 }
              )
            , ( Node { tag = NUM, value = 6 }
              , Node { tag = OPP, value = 7 }
              )
            , ( Node { tag = NUM, value = 8 }
              , Node { tag = OPP, value = 9 }
              )
            , ( Node { tag = NUM, value = 8 }
              , Node { tag = OPP, value = 9 }
              )
            , ( Node { tag = NUM, value = 8 }
              , Node { tag = OPP, value = 9 }
              )
            ]
          (rAddr, outputs) = P.unzip $ simulateN @System 8 load $ P.zip rData inputs
          expectedRAddr :: [Integer]
          expectedRAddr = [2, 5, 5, 13, 34, 34, 34, 0]
          expectedOutputs :: [LoadOut (Integer)]
          expectedOutputs =
            [ LoadOut
                { node1 = Node { tag = ERA, value = 0 }
                , node2 = Node { tag = ERA, value = 0 }
                , aux1 = Node { tag = ERA, value = 0 }
                , aux2 = Node { tag = ERA, value = 0 }
                , valid = False
                }
            , LoadOut
                { node1 = Node { tag = NUM, value = 1 }
                , node2 = Node { tag = OPP, value = 2 }
                , aux1 = Node { tag = NUM, value = 7 }
                , aux2 = Node { tag = OPP, value = 4 }
                , valid = True
                }
            , LoadOut
                { node1 = Node { tag = NUM, value = 3 }
                , node2 = Node { tag = OPP, value = 5 }
                , aux1 = Node { tag = NUM, value = 11 }
                , aux2 = Node { tag = OPP, value = 6 }
                , valid = True
                }
            , LoadOut
                { node1 = Node { tag = ERA, value = 0 }
                , node2 = Node { tag = ERA, value = 0 }
                , aux1 = Node { tag = NUM, value = 11 }
                , aux2 = Node { tag = OPP, value = 6 }
                , valid = False
                }
            , LoadOut
                { node1 = Node { tag = NUM, value = 8 }
                , node2 = Node { tag = OPP, value = 13 }
                , aux1 = Node { tag = NUM, value = 6 }
                , aux2 = Node { tag = OPP, value = 7 }
                , valid = False
                }
            , LoadOut
                { node1 = Node { tag = NUM, value = 8 }
                , node2 = Node { tag = OPP, value = 13 }
                , aux1 = Node { tag = NUM, value = 8 }
                , aux2 = Node { tag = OPP, value = 9 }
                , valid = True
                }
            , LoadOut
                { node1 = Node { tag = NUM, value = 21 }
                , node2 = Node { tag = OPP, value = 34 }
                , aux1 = Node { tag = NUM, value = 8 }
                , aux2 = Node { tag = OPP, value = 9 }
                , valid = True
                }
            , LoadOut
                { node1 = Node { tag = ERA, value = 0 }
                , node2 = Node { tag = ERA, value = 0 }
                , aux1 = Node { tag = NUM, value = 8 }
                , aux2 = Node { tag = OPP, value = 9 }
                , valid = False
                }
            ]
      rAddr `shouldBe` expectedRAddr
      outputs `shouldBe` expectedOutputs
