"""
trees with similar subtrees (similar = obtainable by translations)
compressed.
"""
from jimn.tree import Tree
from jimn.point import Point


class TranslatedTree(Tree):
    """
    base class for jimn trees.
    """
    def __init__(self, content=None):
        super().__init__(content)
        # we compress translated subtrees together.
        # apply following list if translations to self
        # to find all real trees
        self.translations = [Point([0, 0])]

    def add_translation(self, translation_vector):
        """
        mark node (and whole subtree) as duplicated for
        given translation vector.
        """
        self.translations.append(translation_vector)

    def copy_translations(self, other):
        """
        get same translations to apply as other.
        used to keep translations when converting trees
        """
        self.translations = other.translations

    def dot_label(self):
        """
        return label used in dot file when saving node self.
        """
        label = super().dot_label()
        if len(self.translations) == 1:
            return label
        elif len(self.translations) < 10:
            return label + "(" + \
                ",".join(str(t) for t in self.translations) + ")"
        else:
            return label + "(...)"
