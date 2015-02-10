class segment:
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2
    def __str__(self):
        return "[{} ; {}]".format(str(self.p1), str(self.p2))
