import Jimn.Point
import Jimn.PointsRounder
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Points Rounder" $ do
    it "fuses nearby points" $ do
      let p1 = Point [0.13, 0.13]
          p2 = Point [0.15, 0.15]
          p3 = Point [0.18, 0.18]
          p4 = Point [0.09, 0.18]
          rounder = empty 1 2
          (newRounder, _) = add rounder p1 in do
            Jimn.PointsRounder.lookup newRounder p2 `shouldBe` Just p1
            Jimn.PointsRounder.lookup newRounder p3 `shouldBe` Just p1
            Jimn.PointsRounder.lookup newRounder p4 `shouldBe` Just p1
