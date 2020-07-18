module Language.FiniteStateSpec
  ( spec
  )
where

import           Test.Hspec
import           Language.FiniteState

spec :: Spec
spec = do
  describe "DFA" $ do
    it "can evaluate a deterministic two state language language" $ do
      let dfa :: DFA String Integer
          dfa = DFA ["Even", "Odd"] [0, 1] delta "Even" ["Even"]
           where
            delta "Even" 1 = "Odd"
            delta "Odd"  1 = "Even"
            delta s      _ = s

      evalDFA dfa [0, 0, 0] `shouldBe` True
      evalDFA dfa [0, 0, 1] `shouldBe` False
      evalDFA dfa [1, 1, 1] `shouldBe` False
      evalDFA dfa [1, 1, 0] `shouldBe` True

  describe "NFA" $ do
    it "can evaluate a non-deterministic three state language" $ do
      let nfa :: NFA String Integer
          nfa = NFA ["Even", "Odd", "Neither"]
                    [0, 1, 2]
                    delta
                    "Even"
                    ["Even", "Odd"]
           where
            delta "Even"    1 = ["Odd"]
            delta "Odd"     1 = ["Odd"]
            delta "Neither" 1 = ["Even", "Odd"]
            delta _         2 = ["Neither"]
            delta s         _ = [s]

      evalNFA nfa [0, 1, 0] `shouldBe` True
      evalNFA nfa [1, 0, 1] `shouldBe` True
      evalNFA nfa [2, 0, 0] `shouldBe` False
      evalNFA nfa [2, 1, 1] `shouldBe` True
      evalNFA nfa [2, 1, 2] `shouldBe` False


