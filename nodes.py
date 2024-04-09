class NumNode:
    kind = "num"

    def __init__(self, n: float):
        self.n = n

class StrNode:
    kind = "str"

    def __init__(self, s: str):
        self.s = s

class IdentNode:
    kind = "ident"
    
    def __init__(self, i: str):
        self.i = i

class UnOpNode:
    kind = "unop"
    
    def __init__(self, op: str, other):
        self.op = op
        self.other = other

class BinOpNode:
    kind = "binop"
    
    def __init__(self, op: str, left, right):
        self.op = op
        self.left = left
        self.right = right

class LineNode:
    kind = "line"

    def __init__(self, line, var: str, expr):
        self.line = line
        self.var = var
        self.expr = expr