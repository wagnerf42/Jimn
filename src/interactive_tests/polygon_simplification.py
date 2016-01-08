#!/usr/bin/env python3

from jimn.point import Point
from jimn.polygon import polygon
from jimn.displayable import tycat

p = polygon([
    Point([-1.5, 0.2071000039577484]),
    Point([-1.29497096657753, 0.7020999744534493]),
    Point([-1.2928999662399292, 0.707099974155426]),
    Point([-1.1728129839897157, 0.9970709997415542]),
    Point([-1.1715999841690063, 1.0]),
    Point([-1.1728129839897157, 1.0029289996623993]),
    Point([-1.2928999662399292, 1.2928999662399292]),
    Point([-1.0029289996623993, 1.1728129839897157]),
    Point([-1.0, 1.1715999841690063]),
    Point([-0.7100289744138718, 1.2916869664192199]),
    Point([-0.707099974155426, 1.2928999662399292]),
    Point([-0.2121000036597252, 1.4979289996623992]),
    Point([-0.2071000039577484, 1.5]),
    Point([-0.002071000039577484, 1.005]),
    Point([0.0, 1.0]),
    Point([0.20502900391817092, 1.495]),
    Point([0.2071000039577484, 1.5]),
    Point([0.7020999744534493, 1.29497096657753]),
    Point([0.707099974155426, 1.2928999662399292]),
    Point([0.9970709997415542, 1.1728129839897157]),
    Point([1.0, 1.1715999841690063]),
    Point([1.2899709665775299, 1.2916869664192199]),
    Point([1.2928999662399292, 1.2928999662399292]),
    Point([1.2916869664192199, 1.2899709665775299]),
    Point([1.1715999841690063, 1.0]),
    Point([1.2916869664192199, 0.7100289744138718]),
    Point([1.2928999662399292, 0.707099974155426]),
    Point([1.4979289996623992, 0.2121000036597252]),
    Point([1.5, 0.2071000039577484]),
    Point([1.495, 0.20502900391817092]),
    Point([1.0, 0.0]),
    Point([1.495, -0.20502900391817092]),
    Point([1.5, -0.2071000039577484]),
    Point([1.4979289996623992, -0.2121000036597252]),
    Point([1.2928999662399292, -0.707099974155426]),
    Point([1.2916869664192199, -0.7100289744138718]),
    Point([1.1715999841690063, -1.0]),
    Point([1.2916869664192199, -1.2899709665775299]),
    Point([1.2928999662399292, -1.2928999662399292]),
    Point([1.2899709665775299, -1.2916869664192199]),
    Point([1.0, -1.1715999841690063]),
    Point([0.9970709997415542, -1.1728129839897157]),
    Point([0.707099974155426, -1.2928999662399292]),
    Point([0.7020999744534493, -1.29497096657753]),
    Point([0.2071000039577484, -1.5]),
    Point([0.20502900391817092, -1.495]),
    Point([0.0, -1.0]),
    Point([-0.002071000039577484, -1.005]),
    Point([-0.2071000039577484, -1.5]),
    Point([-0.2121000036597252, -1.4979289996623992]),
    Point([-0.707099974155426, -1.2928999662399292]),
    Point([-0.7100289744138718, -1.2916869664192199]),
    Point([-1.0, -1.1715999841690063]),
    Point([-1.0029289996623993, -1.1728129839897157]),
    Point([-1.2928999662399292, -1.2928999662399292]),
    Point([-1.1728129839897157, -1.0029289996623993]),
    Point([-1.1715999841690063, -1.0]),
    Point([-1.1728129839897157, -0.9970709997415542]),
    Point([-1.2928999662399292, -0.707099974155426]),
    Point([-1.29497096657753, -0.7020999744534493]),
    Point([-1.5, -0.2071000039577484]),
    Point([-1.005, -0.002071000039577484]),
    Point([-1.0, 0.0]),
    Point([-1.005, 0.002071000039577484])
])

tycat(p, p.points)
p.remove_useless_points()
tycat(p, p.points)
