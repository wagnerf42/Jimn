import Point
import Segment
import Facet


main=
  print(slice [f1, f2] 0.5) where
    p1 = Point [0,0,-1]
    p2 = Point [1,0,-1]
    p3 = Point [0.5,0,1]
    p4 = Point [0.5,0,2]
    f1 = Facet p1 p2 p3
    f2 = Facet p1 p2 p4
