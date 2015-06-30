from jimn.holed_polygon import holed_polygon


class translated_holed_polygon(holed_polygon):
    def __init__(self, original, translated):
        self.copy(translated)
        self.original = original
        a1, a2 = [hp.polygon.get_points()[0] for hp in [self, self.original]]
        self.translation_vector = a2 - a1
