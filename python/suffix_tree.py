class Node:
    def __init__(self, i, j=None, children=None):
        self.i = i
        self.j = j
        self.children = children if children else {}

    def __getitem__(self, c):
        return self.children[c]

    def __repr__(self):
        return "Node("+str(self.i)+", "+str(self.j)+", children="+str(self.children)+")"


class SuffixTree:
    """

    see: http://stackoverflow.com/questions/9452701/ukkonens-suffix-tree-algorithm-in-plain-english
    """
    def __init__(self, s):
        self.root = Node(0,0)
        self.s = s
        self.insert(s)

    def insert(self, s):
        print("="*90)
        root = self.root
        remainder = 0
        n = len(s)

        ## initialize active-point
        active_node = root
        active_edge = None
        active_length = 0

        for i in range(n):
            remainder += 1
            c = s[i]

            ## for each character, we'll do one of:
            ##   - add a child node to an existing node
            ##   - add a new split point and a new node
            ##   - continue traversing existing nodes


            while remainder > 0:
                print("~"*60)
                print("i=",i,"c=",c)
                if active_length==0:
                    if c in active_node.children:
                        active_edge = c
                        active_length += 1
                        break
                    else:
                        active_node.children[c] = Node(i)
                        remainder -= 1
                else:
                    next_node = active_node[active_edge]
                    j = next_node.i + active_length
                    if next_node.j is None or j < next_node.j:
                        if c == s[j]:
                            ## keep going along the currently active edge from active_node
                            ## to next_node
                            active_length += 1
                            break
                        else:
                            print('splitting at', next_node, s[next_node.i], active_edge, active_length)
                            ## split
                            old_tail = Node(j, next_node.j, children=next_node.children)
                            next_node.children = {c:Node(i), s[j]:old_tail}
                            next_node.j = j
                            active_node = root
                            active_length -= 1
                            remainder -= 1
                            active_edge = s[i-remainder+1]

        if remainder:
            print('cleaning up remainder', remainder)
            next_node = active_node[active_edge]
            j = next_node.i + active_length
            print('splitting at', next_node, s[next_node.i:next_node.j], active_edge, active_length)
            ## split
            old_tail = Node(j, next_node.j, children=next_node.children)
            print("creating dummy node", i+1)
            next_node.children = {'$$':Node(i+1), s[j]:old_tail}
            next_node.j = j
            active_node = root
            active_length -= 1
            remainder -= 1
            active_edge = None

    def __contains__(self, key):
        node = self.root
        active_length = 0
        for c in key:
            ## Check if we've completely traversed an edge
            ## and need to move to the next node. This should
            ## happen on the first iteration to get off of the
            ## root node.
            if node.j is not None and active_length >= node.j:
                if c in node.children:
                    node = node.children[c]
                    active_length = 0
                else:
                    return False
            ## 'j' is the index into the original string. Compare
            ## characters unless j is off the end of the string,
            ## meaning that the key is too long, for example asking
            ## if 'abcd' is a suffix of 'abc'.
            j = active_length + node.i
            if j >= len(self.s) or c!=self.s[j]:
                return False
            active_length += 1
        ## look for the special leaf node that signals end-of-string in
        ## cases like "abca" that do not end on a unique character
        if node.j is not None and active_length+node.i == node.j and '$$' in node.children:
            return True
        return j+1 == len(self.s)

    def __iter__(self):
        return self._dfs(self.root)

    def _dfs(self, node):
        """
        Generate suffixes by depth-first-search
        """
        substr = self.s[node.i:node.j]
        if len(node.children) == 0:
            yield substr
        else:
            for child in node.children.values():
                for suffix in self._dfs(child):
                    yield substr + suffix


def verify_suffix_tree(st, s=None):
    if s is None:
        s = st.s

    ## make sure that every suffix in the suffix tree is really a
    ## suffix of the original string
    for suffix in st:
        if not s.endswith(suffix):
            print('"%s" is not a suffix of "%s"'%(suffix, s))

    ## make sure every suffix of the string is in the suffix tree
    n = len(s)
    for i in range(n):
        suffix = s[n-i-1:]
        if not suffix in st:
            print('"%s" missing from suffix tree'%(suffix))            


if __name__ == "__main__":

    ## test step 1
    st = SuffixTree('abc')
    assert sorted(st.root.children.keys()) == ['a','b','c']
    assert st.root['a'].i == 0
    assert st.root['b'].i == 1
    assert st.root['c'].i == 2
    assert 'abc' in st
    assert 'bc' in st
    assert 'c' in st
    assert 'zqx' not in st
    assert 'aba' not in st
    assert 'abcd' not in st
    assert 'ab' not in st
    for suffix in st:
        print(suffix)
    verify_suffix_tree(st)

    ## test step 2
    st = SuffixTree('abcax')
    assert st.root['a'].i == 0
    assert st.root['a'].j == 1
    assert st.root['b'].i == 1
    assert st.root['c'].i == 2
    assert st.root['x'].i == 4
    assert st.root['a'].children['x'].i == 4
    assert sorted(st.root.children.keys()) == ['a','b','c','x']
    for suffix in st:
        print(suffix)
    verify_suffix_tree(st)

    ## test step 3
    st = SuffixTree('abca')
    for suffix in st:
        print(suffix)
    verify_suffix_tree(st)

    ## test step 4
    st = SuffixTree('abcabx')
    assert st.root['a'].i == 0
    assert st.root['b'].i == 1
    assert st.root['c'].i == 2
    assert st.root['x'].i == 5
    assert sorted(st.root.children.keys()) == ['a','b','c','x']

    ## test step 5
    # st = SuffixTree('abcabxabcd')

