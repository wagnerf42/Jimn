class edge:
    def __init__(self, start_vertex, end_vertex, real_path):
        self.vertices = [start_vertex, end_vertex]
        self.path = real_path

    def get_weight(self):
        return self.path.length()

    def get_endpoint(self, index):
        return self.vertices[index]

    def get_destination(self):
        return self.vertices[1]
