module Language.Datalog.ProgramSpec
  ( spec
  )
where

import           Test.Hspec
import           Language.Datalog.Program       ( Atom(..)
                                                , Term(..)
                                                , Rule(..)
                                                , querySubstitutions
                                                )

spec :: Spec
spec = do
  -- State some facts; "xerces" is parent to "brooke", "brooke" is parent to
  -- "damocles".
  let facts = fmap
        (\terms -> Rule (Atom "parent" terms) [])
        [[Sym "xerces", Sym "brooke"], [Sym "brooke", Sym "damocles"]]

  -- Define rules; "X" is an ancestor of "Y" if "X" is a parent of "Y", or "X"
  -- is a parent of some "Z" who is an ancestor of "Y".
  let
    rules =
      [ Rule (Atom "ancestor" [Var "X", Var "Y"])
             [Atom "parent" [Var "X", Var "Y"]]
      , Rule
        (Atom "ancestor" [Var "X", Var "Z"])
        [Atom "parent" [Var "X", Var "Y"], Atom "ancestor" [Var "Y", Var "Z"]]
      ]

  let queries =
        -- Query should substitute intermediate for brooke and damocles, since
        -- they're both ancestors of xerces.
        [ Rule (Atom "query1" [Var "Intermediate"])
               (fmap (Atom "ancestor") [[Sym "xerces", Var "Intermediate"]])
        -- Query is satisfied by an empty substitution since xerces is an
        -- ancestor of brooke.
        , Rule (Atom "query2" []) [Atom "ancestor" [Sym "xerces", Sym "brooke"]]
        -- Query cannot be satisfied by any substitution.
        , Rule (Atom "query3" []) [Atom "ancestor" [Sym "brooke", Sym "xerces"]]
        ]

  let prog = facts <> rules <> queries

  describe "querySubstitutions" $ do
    it "can replace intermediates" $ do
      let expected =
            [ [(Var "Intermediate", Sym "brooke")]
            , [(Var "Intermediate", Sym "damocles")]
            ]
      (querySubstitutions "query1" prog) `shouldMatchList` expected

    it "returns a single empty substitution when query is satisfied" $ do
      (querySubstitutions "query2" prog) `shouldBe` [[]]

    it "returns no substitutions if none satisfy query" $ do
      (querySubstitutions "query3" prog) `shouldBe` []
