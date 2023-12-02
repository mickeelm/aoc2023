import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "part1" $ do
    it "example" $ do
      readExampleInput >>= \input -> (solve1 (lines input)) `shouldBe` (142 :: Int)
    it "real" $ do
      readRealInput >>= \input -> (solve1 (lines input)) `shouldBe` (53386 :: Int)

  describe "part2" $ do
    it "example" $ do
      readExampleInput2 >>= \input -> (solve2 (lines input)) `shouldBe` (281 :: Int)
    it "real" $ do
      readRealInput >>= \input -> (solve2 (lines input)) `shouldBe` (53312 :: Int)

readRealInput :: IO String
readRealInput = readFile "input.txt"

readExampleInput :: IO String
readExampleInput = readFile "example_input.txt"

readExampleInput2 :: IO String
readExampleInput2 = readFile "example_input_2.txt"