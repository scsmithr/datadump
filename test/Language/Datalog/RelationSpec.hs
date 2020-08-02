module Language.Datalog.RelationSpec
  ( spec
  )
where

import           Test.Hspec
import           Language.Datalog.Relation

spec :: Spec
spec = do
  describe "unionRelation" $ do
    it "unions two relations" $ do
      let r1       = fromTuples [Tuple [("k1", 5)]]
          r2       = fromTuples [Tuple [("k2", 6)]]
          expected = fromTuples [Tuple [("k1", 5)], Tuple [("k2", 6)]]
      unionRelation r1 r2 `shouldBe` expected

    it "removes duplicate tuples" $ do
      let r1 = fromTuples [Tuple [("k1", 4)]]
          r2 = fromTuples [Tuple [("k1", 4)]]
      unionRelation r1 r2 `shouldBe` r1

  describe "joinTuples" $ do
    it "produces a tuple containing matching keys" $ do
      let t1       = Tuple [("k1", 3), ("k2", 9)]
          t2       = Tuple [("k2", 1), ("k3", 0)]
          expected = Tuple [("k2", (9, 1))]
      joinTuples (,) t1 t2 `shouldBe` expected

    it "produces an empty tuple on no matches" $ do
      let t1 = Tuple [("k1", 4)]
          t2 = Tuple [("k2", 4)]
      joinTuples (,) t1 t2 `shouldBe` Tuple []

  describe "joinRelations" $ do
    it "produces an empty relation when disjoint" $ do
      let r1 = fromTuples [Tuple [("k1", 5)]]
          r2 = fromTuples [Tuple [("k2", 6)]]
      joinRelations (,) r1 r2 `shouldBe` emptyRelation

    it "produces new relation with new value" $ do
      let r1       = fromTuples [Tuple [("k1", 5)], Tuple [("k2", 8)]]
          r2       = fromTuples [Tuple [("k3", 9)], Tuple [("k1", 6)]]
          expected = fromTuples [Tuple [("k1", (5, 6))]]
      joinRelations (,) r1 r2 `shouldBe` expected

    it "produces a new relation with many key matches" $ do
      let
        r1       = fromTuples [Tuple [("k1", 5)], Tuple [("k3", 8)]]
        r2       = fromTuples [Tuple [("k3", 9)], Tuple [("k1", 6)]]
        expected = fromTuples [Tuple [("k1", (5, 6))], Tuple [("k3", (8, 9))]]
      joinRelations (,) r1 r2 `shouldBe` expected

