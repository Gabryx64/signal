import ply.lex as lex
import ply.yacc as yacc
from nodes import NumNode, IdentNode, UnOpNode, BinOpNode, LineNode, StrNode
from typing import List, Callable, Dict
import sys

tokens = [
    "NUM",
    "IDENT",
    "STR",
    "PLUS",
    "MINUS",
    "MUL",
    "DIV",
    "MOD",
    "POW",
    "ROOT",
    "NEG",
    "LPAREN",
    "RPAREN",
    "COLON",
    "NEWLINE",
]

t_ignore = " \t"

t_PLUS = r"\+"
t_MINUS = r"\-"
t_MUL = r"\*"
t_DIV = r"\/"
t_MOD = r"\%"
t_POW = r"\*\*"
t_ROOT = r"\//"
t_NEG = r"\~"
t_LPAREN = r"\("
t_RPAREN = r"\)"
t_COLON = r"\:"

def t_NUM(t):
    r"\d+|\d+.\d+"
    t.type = "NUM"
    return t

def t_IDENT(t):
    r"[a-zA-Z_?&$][a-zA-Z_?&$0-9]*"
    t.type = "IDENT"
    return t

def t_STR(t):
    r"'[^\n']*'"
    t.type = "STR"
    t.value = t.value[1:-1]
    return t

def t_newline(t):
    r"\n+"
    t.type = "NEWLINE"
    t.lexer.lineno += len(t.value)
    return t

def t_error(t):
    print(f"Syntax Error: '{t.value[0]}'!")
    t.lexer.skip(1)

precedence = [
    ("left", "PLUS", "MINUS"),
    ("left", "MUL", "DIV", "MOD"),
    ("left", "POW", "ROOT"),
]

def p_atom_expr(p):
    """
    atom : LPAREN expr RPAREN
    """
    p[0] = p[2]

def p_atom_unop(p):
    """
    atom : NEG atom
         | COLON atom
    """
    p[0] = UnOpNode(p[1], p[2])

def p_atom_ident(p):
    """
    atom : IDENT
    """
    p[0] = IdentNode(p[1])

def p_atom_num(p):
    """
    atom : NUM
    """
    p[0] = NumNode(float(p[1]))

def p_atom_str(p):
    """
    atom : STR
    """
    p[0] = StrNode(p[1])

def p_expr(p):
    """
    expr : expr PLUS expr
         | expr MINUS expr
         | expr MUL expr
         | expr DIV expr
         | expr MOD expr
         | expr POW expr
         | expr ROOT expr
    """
    p[0] = BinOpNode(p[2], p[1], p[3])

def p_expr_atom(p):
    """
    expr : atom
    """
    p[0] = p[1]

def p_line(p):
    """
    line : atom IDENT COLON expr
    """
    p[0] = LineNode(p[1], p[2], p[4])

def p_emptyline(p):
    """
    line : NEWLINE
    """
    p[0] = p[1]

def p_lines(p):
    """
    lines : lines line
    """
    p[0] = p[1] + [p[2]]

def p_lines_line(p):
    """
    lines : line
          | empty
    """
    p[0] = [p[1]]

def p_empty(p):
    """
    empty : NEWLINE
    """
    p[0] = None

def p_signal(p):
    r"""
    signal : lines
    """
    run(p[1])

vars: Dict[str, Callable[[Callable | None], any]] = {}

def eval(node: NumNode | IdentNode | UnOpNode | BinOpNode) -> Callable[[Callable | None], any]:
    match node.kind:
        case "num":
            def ret(_1, _2):
                return node.n
        case "str":
            def ret(_1, _2):
                return node.s
        case "ident":
            def ret(sink, prev):
                if prev is not None and node.i == prev[0] and sink is None:
                    return prev[1]
                if node.i == "?":
                    return int(input(""))
                if node.i == "$?":
                    inpt = ""
                    while inpt != "":
                        inpt = input("")
                    return ord(inpt[0])
                return vars[node.i](sink)
        case "unop":
            match node.op:
                case "~":
                    def ret(sink, prev):
                        return -eval(node.other)(sink, prev)
                case ":":
                    def ret(_, prev):
                        return eval(node.other)(None, prev)
        case "binop":
            match node.op:
                case "+":
                    def ret(sink, prev):
                        left = eval(node.left)(sink, prev)
                        right = eval(node.right)(sink, prev)
                        if isinstance(left, str) or isinstance(right, str):
                            left = left if isinstance(left, str) else f"{left:g}"
                            right = right if isinstance(right, str) else f"{right:g}"
                        
                        return left + right
                case "-":
                    def ret(sink, prev):
                        return eval(node.left)(sink, prev) - eval(node.right)(sink, prev)
                case "*":
                    def ret(sink, prev):
                        return eval(node.left)(sink, prev) * eval(node.right)(sink, prev)
                case "/":
                    def ret(sink, prev):
                        return eval(node.left)(sink, prev) / eval(node.right)(sink, prev)
                case "%":
                    def ret(sink, prev):
                        return eval(node.left)(sink, prev) % eval(node.right)(sink, prev)
                case "**":
                    def ret(sink, prev):
                        return eval(node.left)(sink, prev) ** eval(node.right)(sink, prev)
                case "//":
                    def ret(sink, prev):
                        return eval(node.left)(sink, prev) ** (1 / eval(node.right)(sink, prev))
    
    def retfn(sink: Callable | None, prev=None) -> any:
        if retfn.computed is not None:
            if sink:
                retfn.sinks.add(sink)
                sink.deps.add(retfn)
            return retfn.computed
        
        for sink in retfn.sinks:
            sink.computed = None
        
        if sink is not None:
            retfn.sinks.add(sink)
            sink.deps.add(retfn)

        if not hasattr(retfn, "prev"):
            retfn.prev = prev
    
        retfn.computed = ret(sink, retfn.prev)
        return retfn.computed
        
    retfn.sinks = set()
    retfn.deps = set()
    retfn.computed = None
    return retfn

def run(linesnodes: List[LineNode | str]) -> Dict[any, Callable[[Callable | None], any]]:
    linesnodes = list(filter(lambda x: not isinstance(x, str), linesnodes))
    def linevals():
        if linevals.computed is not None:
            return linevals.computed
        
        computed = {}
        fully_computed = True
        for linenodes in linesnodes:
            def computegen(linenodes):
                def compute():
                    computedfn = eval(linenodes.expr)

                    if linenodes.var in vars:
                        computedfn.sinks = vars[linenodes.var].sinks
                        computedfn.prev = (linenodes.var, vars[linenodes.var].computed)
                    else:
                        computedfn.sinks = set()

                    for sink in computedfn.sinks:
                        sink.computed = None

                    vars[linenodes.var] = computedfn
                return compute
            
            try:
                linenum = eval(linenodes.line)(linevals)
                computed[linenum] = computegen(linenodes)
            except KeyError:
                fully_computed = False

        if fully_computed:
            linevals.computed = computed
            
        return computed

    linevals.computed = None
    linevals.deps = set()

    i = -1
    while True:
        i += 1
        lines = linevals()

        if i in lines:
            lines[i]()
            if "?" in vars:
                val = vars.pop("?")(None)
                val = val if isinstance(val, str) else f"{val:g}"
                print(val)
            if "??" in vars:
                val = vars.pop("?")(None)
                val = val if isinstance(val, str) else f"{val:g}"
                print(val, end="")
            if "$?" in vars:
                print(chr(int(vars.pop("$?")(None))))
            if "&" in vars:
                retcode = vars.pop("&")(None)
                if isinstance(retcode, float) and retcode.is_integer():
                    sys.exit(int(retcode))
                else:
                    sys.exit(int(retcode))

lexer = lex.lex()
parser = yacc.yacc(start="signal")

for arg in sys.argv[1:]:
    with open(arg, "r") as f:
        parser.parse(f.read())