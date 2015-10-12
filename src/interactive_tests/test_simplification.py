#!/usr/bin/env python3

from jimn.point import point
from jimn.polygon import polygon
from jimn.displayable import tycat

p = polygon([
    point([-1.5, 0.2071000039577484]),
    point([-1.29497096657753, 0.7020999744534493]),
    point([-1.2928999662399292, 0.707099974155426]),
    point([-1.1728129839897157, 0.9970709997415542]),
    point([-1.1715999841690063, 1.0]),
    point([-1.1728129839897157, 1.0029289996623993]),
    point([-1.2928999662399292, 1.2928999662399292]),
    point([-1.0029289996623993, 1.1728129839897157]),
    point([-1.0, 1.1715999841690063]),
    point([-0.7100289744138718, 1.2916869664192199]),
    point([-0.707099974155426, 1.2928999662399292]),
    point([-0.2121000036597252, 1.4979289996623992]),
    point([-0.2071000039577484, 1.5]),
    point([-0.002071000039577484, 1.005]),
    point([0.0, 1.0]),
    point([0.20502900391817092, 1.495]),
    point([0.2071000039577484, 1.5]),
    point([0.7020999744534493, 1.29497096657753]),
    point([0.707099974155426, 1.2928999662399292]),
    point([0.9970709997415542, 1.1728129839897157]),
    point([1.0, 1.1715999841690063]),
    point([1.2899709665775299, 1.2916869664192199]),
    point([1.2928999662399292, 1.2928999662399292]),
    point([1.2916869664192199, 1.2899709665775299]),
    point([1.1715999841690063, 1.0]),
    point([1.2916869664192199, 0.7100289744138718]),
    point([1.2928999662399292, 0.707099974155426]),
    point([1.4979289996623992, 0.2121000036597252]),
    point([1.5, 0.2071000039577484]),
    point([1.495, 0.20502900391817092]),
    point([1.0, 0.0]),
    point([1.495, -0.20502900391817092]),
    point([1.5, -0.2071000039577484]),
    point([1.4979289996623992, -0.2121000036597252]),
    point([1.2928999662399292, -0.707099974155426]),
    point([1.2916869664192199, -0.7100289744138718]),
    point([1.1715999841690063, -1.0]),
    point([1.2916869664192199, -1.2899709665775299]),
    point([1.2928999662399292, -1.2928999662399292]),
    point([1.2899709665775299, -1.2916869664192199]),
    point([1.0, -1.1715999841690063]),
    point([0.9970709997415542, -1.1728129839897157]),
    point([0.707099974155426, -1.2928999662399292]),
    point([0.7020999744534493, -1.29497096657753]),
    point([0.2071000039577484, -1.5]),
    point([0.20502900391817092, -1.495]),
    point([0.0, -1.0]),
    point([-0.002071000039577484, -1.005]),
    point([-0.2071000039577484, -1.5]),
    point([-0.2121000036597252, -1.4979289996623992]),
    point([-0.707099974155426, -1.2928999662399292]),
    point([-0.7100289744138718, -1.2916869664192199]),
    point([-1.0, -1.1715999841690063]),
    point([-1.0029289996623993, -1.1728129839897157]),
    point([-1.2928999662399292, -1.2928999662399292]),
    point([-1.1728129839897157, -1.0029289996623993]),
    point([-1.1715999841690063, -1.0]),
    point([-1.1728129839897157, -0.9970709997415542]),
    point([-1.2928999662399292, -0.707099974155426]),
    point([-1.29497096657753, -0.7020999744534493]),
    point([-1.5, -0.2071000039577484]),
    point([-1.005, -0.002071000039577484]),
    point([-1.0, 0.0]),
    point([-1.005, 0.002071000039577484])
])

tycat(p)
p.remove_useless_points()
tycat(p)
