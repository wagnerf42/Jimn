from collections import deque


class tree:

    def get_children(self):
        return self.children

    def depth_first_exploration(self):
        stack = []
        stack.append(self)
        while stack:
            node = stack.pop()
            yield node
            stack.extend(node.get_children())

    def breadth_first_exploration(self):
        d = deque()
        d.append(self)
        while d:
            node = d.popleft()
            yield node
            d.extend(node.get_children())
