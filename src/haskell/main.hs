import Point
import Segment

main =
  print(svg v "red" p1) where
    p1 = Point [1,4]
    p2 = Point [3,2]
    s = Segment p1 p2
    b = Segment.box s
    v = view [640, 480] b
