from jimn.holed_polygon import holed_polygon
from jimn.tree import tree


class polygon_tree(tree):
    def add_child(self, polygon, height, holes):
        new_child = polygon_tree(holed_polygon(polygon, height, holes))
        self.children.append(new_child)
        return new_child

    def normalize_polygons(self):
        """call normalize method on each polygon of the tree
        this is a prerequisite for translated polygon identifications
        """
        for node in self.depth_first_exploration():
            new_polygon = node.content
            if new_polygon is not None:
                new_polygon.normalize()
