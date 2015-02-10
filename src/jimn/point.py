class point:
    def __init__(self, *arg):
        l = []
        for x in arg:
            l.append(x)
        self.coord = l
    def __str__(self):
        s = ""
        s += "("
        for x in self.coord:
            s += str(x) + ", "
        s = s[:-2]
        s += ")"
        return s
