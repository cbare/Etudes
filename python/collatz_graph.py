import pygraphviz as pgv

def collatz_graph(n, filename=None):
    """
    Use pygraphviz and Graphviz to visualize Collatz sequences for numbers
    up to n.
    """
    g = pgv.AGraph()
    g.add_node(1, fillcolor='#19a8ff30', style='filled')
    if n < 1: return g

    for x in range(2, n+1):
        a = x
        while a>1:
            if a % 2 == 0:
                b = a//2
                g.add_node(a, fillcolor='#19a8ff30', style='filled')
                g.add_edge(a,b, color='#19a8ff', dir='forward')
            else:
                b = 3*a + 1
                g.add_node(a, fillcolor='#e3423430', style='filled')
                g.add_edge(a,b, color='#e34234', dir='forward')
            a = b

    filename = filename or f'collatz_graph_{n}.svg'

    g.layout(prog='dot')
    g.draw(filename)

collatz_graph(1000)
