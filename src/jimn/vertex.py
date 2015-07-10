from jimn.point import point


class vertex(point):
    def __init__(self, position_point, links=[]):
        super().__init__(position_point.get_coordinates())
        self.links = links

    def add_link(self, link):
        self.links.append(link)
