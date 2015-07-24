from collections import deque

"""
abstract tree class.
to be derived from
"""
class tree:

    def get_children(self):
        """
        return children array of current node
        """
        return self.children

    def depth_first_exploration(self):
        """
        iterator : depth first exploration.
        use it as : for n in t.depth_first_exploration()
        """
        stack = []
        stack.append(self)
        while stack:
            node = stack.pop()
            yield node
            stack.extend(node.get_children())

    def breadth_first_exploration(self):
        """
        iterator : breadth first exploration.
        use it as : for n in t.breadth_first_exploration()
        """
        d = deque()
        d.append(self)
        while d:
            node = d.popleft()
            yield node
            d.extend(node.get_children())
