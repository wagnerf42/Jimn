from jimn.holed_polygon import holed_polygon


class translated_holed_polygon(holed_polygon):
    def __init__(self, original, translated):
        super().__init__(translated.polygon, translated.height, translated.holes)
        self.original = original
        self.translation_vector = self.compute_translation_vector()

    def compute_translation_vector(self):
        a1, a2 = [hp.polygon.get_points()[0] for hp in [self, self.original]]
        return a2 - a1
