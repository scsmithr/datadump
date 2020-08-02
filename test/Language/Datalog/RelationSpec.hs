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

  describe "joinRelations" $ do
    it "produces relation with zero length tuple when disjoint" $ do
      let r1       = fromTuples [Tuple [("k1", 5)]]
          r2       = fromTuples [Tuple [("k2", 6)]]
          expected = fromTuples [Tuple []]
      joinRelations (,) r1 r2 `shouldBe` expected

    it "produces new relation with new value" $ do
      let r1       = fromTuples [Tuple [("k1", 5)]]
          r2       = fromTuples [Tuple [("k1", 6)]]
          expected = fromTuples [Tuple [("k1", (5, 6))]]
      joinRelations (,) r1 r2 `shouldBe` expected

