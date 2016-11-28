import Jimn.Point
import Jimn.PointsRounder
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Points Rounder" $ do
    it "fuses nearby points (low precision)" $ do
      let points = map Point [[0.13, 0.13], [0.15, 0.15], [0.18, 0.18], [0.09, 0.18]]
          p1 = head points
          rounder = empty 1 2
          (newRounder, _) = add rounder p1 in do
            (map (Jimn.PointsRounder.lookup newRounder) points) `shouldBe` (take (length points) $ repeat $ Just p1)

    it "fuses nearby points (high precision)" $ do
      let points = map Point [[0.10003, 0.10003], [0.10005, 0.10005], [0.10008, 0.10008], [0.09999, 0.10008]]
          p1 = head points
          rounder = empty 4 2
          (newRounder, _) = add rounder p1 in do
            (map (Jimn.PointsRounder.lookup newRounder) points) `shouldBe` (take (length points) $ repeat $ Just p1)
