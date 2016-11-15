import Point
import Segment
import Facet

main =
  print(Facet.intersect 0 f) where
    p1 = Point [0,0,-1]
    p2 = Point [1,0,-1]
    p3 = Point [0.5,0,1]
    f = Facet p1 p2 p3
