# vim : tabstop=4 expandtab shiftwidth=4 softtabstop=4

class segment:
    def __init__(self, p1, p2):
        self.sommets = [p1, p2]
    def __str__(self):
        return "[{} ; {}]".format(str(self.sommets[0]), str(self.sommets[1]))
