from jimn.segment import segment
from jimn.polygon import polygon
from jimn.displayable import tycat
from math import atan2
import time


def hash_points(segments):
    neighbors_by_points = {}
    for s in segments:
        endpoints, reversed_endpoints = s.get_endpoints(), reversed(s.get_endpoints())
        for (p1, p2) in (endpoints, reversed_endpoints):
            if p1 in neighbors_by_points:
                neighbors_by_points[p1].append(p2)
            else:
                neighbors_by_points[p1] = [p2]
            #tycat(segments, *neighbors_by_points[p1])

    return neighbors_by_points

def print_neighbors(neighbors, background):
    for point, neighbors in neighbors.items():
        print("Point : {}".format(point))
        string = " ; ".join(tuple(map(lambda p: str(p), neighbors)))
        print("Neighbors : {}\n".format(string))
        tycat(background, point, polygon(*neighbors))


# angle for 2d points, relative to horizontal line
def angle(*points):
    (x1, y1), (x2, y2) = [p.get_coordinates() for p in points]
    return atan2(y2 - y1, x2 - x1)


def sort_neighbors_by_angle(neighbors_by_points):
    for point, neighbors in neighbors_by_points.items():
        sorted_neighbors = sorted(neighbors, key=lambda neighbor: angle(point, neighbor))
        neighbors_by_points[point] = sorted_neighbors


def sort_points(seg):
    endpoints = seg.get_endpoints()
    sorted_endpoints = sorted(endpoints, key=lambda endpoint: endpoint.get_x())
    return sorted_endpoints


def sort_segments(segments):
    return sorted(segments)


def build_poly(beg_seg, neighbors, marked, background):
    poly = polygon()
    sorted_points = sort_points(beg_seg)
    beg_point = sorted_points[0]
    print("beg_point:", beg_point)
    poly.append(beg_point)
    prec_point = beg_point
    cour_point = sorted_points[1]
    print("cour_point:", cour_point)
    marked[segment(prec_point, cour_point)] = True
    print(cour_point)
    print(prec_point)
    print("\n")
    tycat(background, poly, cour_point)
    while cour_point != beg_point:
        poly.append(cour_point)
        lneighbors = neighbors[cour_point]
        print(cour_point)
        print(prec_point)
        print("\n")
        tycat(background, poly, cour_point, *lneighbors)
        time.sleep(0.5)
        length = len(lneighbors)
        # print(length)
        index = lneighbors.index(prec_point)
        # print(str(index) + "\n")
        next_index = (index+1) % length
        next_point = lneighbors[next_index]
        prec_point = cour_point
        cour_point = next_point
        # print(str(next_point) + "\n")
        marked[segment(prec_point, cour_point)] = True
    return poly
    # raise SystemExit(1)


def build_lpoly(sorted_lseg, neighbors):
    marked = {}
    lpoly = []
    for seg in sorted_lseg:
        if not(seg in marked):
            lpoly.append(build_poly(seg, neighbors, marked, sorted_lseg))
    return lpoly


def build_polygons(segments):
    neighbors = hash_points(segments)
    sort_neighbors_by_angle(neighbors)
    sorted_segments = sort_segments(segments)
    polygons = build_lpoly(sorted_segments, neighbors)
    return polygons
