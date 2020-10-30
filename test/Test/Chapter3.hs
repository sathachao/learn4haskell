module Test.Chapter3
    ( chapter3
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter3


chapter3 :: Spec
chapter3 = describe "Chapter3" $ do
    describe "Chapter3Normal" $ it "" $ True `shouldBe` True
    describe "Chapter3Advanced" $ it "" $ True `shouldBe` True
    describe "Task 2: fight" $ do
        it "knight's attack > monster's health" $ fight (MkKnight 1 2 0) (MkMonster 1 1 5) `shouldBe` 5
        it "knight and monster survives" $ fight (MkKnight 10 2 0) (MkMonster 10 1 5) `shouldBe` 0
        it "monster defeats knight" $ fight (MkKnight 1 1 0) (MkMonster 10 1 5) `shouldBe` (-1)
    describe "Task 4: city" $ do
        it "build castle in a city" $ buildCastle (MkCity Nothing [] Nothing Nothing) "abc"
            `shouldBe` (MkCity (Just (MkCastle "abc")) [] Nothing Nothing)
        it "build house when no houses" $ buildHouse (MkCity Nothing [] Nothing Nothing) houseOfOne
            `shouldBe` (MkCity Nothing [houseOfOne] Nothing Nothing)
        it "build house when houses exist" $ buildHouse (MkCity Nothing [houseOfTwo] Nothing Nothing) houseOfOne
            `shouldBe` (MkCity Nothing [houseOfOne, houseOfTwo] Nothing Nothing)
        it "build wall: should build when has castle and >=10 living people" $ buildWall
            (MkCity (Just (MkCastle "abc")) [houseOfFour, houseOfFour, houseOfTwo] Nothing Nothing)
            `shouldBe` (MkCity (Just (MkCastle "abc")) [houseOfFour, houseOfFour, houseOfTwo] Nothing (Just MkWall))
        it "build wall: fail when no castle" $ buildWall
            (MkCity Nothing [houseOfFour, houseOfFour, houseOfTwo] Nothing Nothing)
            `shouldBe` (MkCity Nothing [houseOfFour, houseOfFour, houseOfTwo] Nothing Nothing)
        it "build wall: fail when <10 living people" $ buildWall
            (MkCity (Just (MkCastle "abc")) [houseOfFour, houseOfFour, houseOfOne] Nothing Nothing)
            `shouldBe` (MkCity (Just (MkCastle "abc")) [houseOfFour, houseOfFour, houseOfOne] Nothing Nothing)
    describe "Task 7" $ do
        it "append gold" $ append (Gold 10) (Gold 32) `shouldBe` (Gold 42)
        it "append some lists" $ append ([1 .. 3] :: [Int]) ([4 .. 6] :: [Int]) `shouldBe` ([1 .. 6] :: [Int])
        it "append some strings" $ append "hello" "world" `shouldBe` "helloworld"
        it "append Nothing Nothing -> Nothing" $ append (Nothing :: Maybe [Int]) (Nothing :: Maybe [Int]) `shouldBe` (Nothing :: Maybe [Int])
        it "append List Nothing -> Nothing" $ append (Just [1] :: Maybe [Int]) Nothing `shouldBe` (Just [1] :: Maybe [Int])
        it "append Nothing List -> Nothing" $ append Nothing (Just [2] :: Maybe [Int]) `shouldBe` (Just [2] :: Maybe [Int])
        it "append List List -> Nothing" $ append (Just [1] :: Maybe [Int]) (Just [2] :: Maybe [Int]) `shouldBe` (Just [1, 2] :: Maybe [Int])
    describe "Task 8" $ do
        it "Saturday is weekend" $ isWeekend Saturday `shouldBe` True
        it "Sunday is weekend" $ isWeekend Sunday `shouldBe` True
        it "Monday is not weekend" $ isWeekend Monday `shouldBe` False
        it "Tuesday is not weekend" $ isWeekend Tuesday `shouldBe` False
        it "nextDay: Monday -> Tuesday" $ nextDay Monday `shouldBe` Tuesday
        it "nextDay: Saturday -> Sunday" $ nextDay Saturday `shouldBe` Sunday
        it "daysToParty: Friday" $ daysToParty Friday `shouldBe` 7
        it "daysToParty: Saturday" $ daysToParty Saturday `shouldBe` 6
        it "daysToParty: Sunday" $ daysToParty Sunday `shouldBe` 5