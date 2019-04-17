

import sys

from enum import Enum, auto
from inspect import isclass

import parse
from scan import PlexToken

class Scope(Enum):
    GLOBAL = auto()
    LOCAL = auto()

class PChecker:
    binop_types = ('+', '-', '*', '/')

    def __init__(self, ast, debug=True):
        self.ast = ast
        self.debug = debug
        self.globalsym = {}
        self.localsym = self.globalsym
        self.sym = self.globalsym
        self.scope = Scope.GLOBAL
        self.func = None
        self.errors = False
        self.quiet = False

    def check(self):
        self.compound(self.ast)
        self.log(self.globalsym)

    def symget(self, name):
        if self.scope == Scope.LOCAL:
            e = self.localsym.get(name)
            if e and not isinstance(e, PlexToken):
                return e
        return self.globalsym.get(name)

    def compound(self, ast):
        switch = {
            'ARRAY': self.arraydecl,
            'GLOBAL': self.globaldecl,
            'LOCAL': self.localdecl,
            'DEFUN': self.defun,
            'PRINT': self.printstmt,
            'RETURN': self.returnstmt,
            'ASSIGN': self.assign,
            'EXCHANGE': self.exchange,
            # 'IF': self.ifstmt,
            # 'WHILE': self.whilestmt,
            # 'FOREACH': self.foreach,
        }
        for s in ast:
            t = s[0].type
            f = switch.get(t, lambda s: self.log("WTF:", s))
            f(s)

    def arraydecl(self, decl):
        _, tok, r, _ = decl
        name = tok.value
        if name in self.localsym:
            self.err(f'redeclaration of array {tok}')
            return

        try:
            lo, hi = self.range(r)
        except SyntaxError as err:
            self.err(f'when parsing range for {tok}:', err)
            return

        # TODO set expression
        a = Array(lo=lo, hi=hi, tok=tok)
        self.localsym[name] = a
        self.log(f'declaring array {a}')

    def range(self, r):
        _, e1, e2 = r
        t1 = self.expr(e1)
        t2 = self.expr(e2)
        if not (self.compatible(t1, IntLit) and self.compatible(t2, IntLit)):
            raise SyntaxError('range endpoints must be constants')
        return t1.tok.value, t2.tok.value

    def globaldecl(self, decl):
        kw, tok, expr = decl
        name = tok.value

        if self.scope == Scope.GLOBAL:
            if name in self.globalsym:
                self.err(f'redeclaration of global variable {tok}')
                return
            if not expr:
                self.err(f'require expression to initialize {tok}')
                return
        else:
            if name not in self.globalsym:
                self.err(f'no such variable {tok} in global scope')
            elif expr:
                self.err(f'initialization of global variable {tok} illegal in local scope')
            elif name in self.localsym:
                local = self.localsym[name]
                self.err(f'cannot use global {tok}, since local {local} in scope')
            else:
            # indicate that the function is using a globally scoped variable
                self.localsym[name] = kw
                self.log(f'using global variable {tok} in {self.func}')
            return

        try:
            val = self.expr(expr)
        except SyntaxError as err:
            self.err(f'when set global {tok}:', err)
            return

        if self.compatible(val, (IntLit, Scalar)):
            rec = Scalar(tok)
        elif self.compatible(val, Tuple):
            rec = val
            rec.tok = tok

        self.localsym[name] = rec
        self.log(f'declared global var {tok}')

    def localdecl(self, decl):
        _, tok, expr = decl
        name = tok.value

        if self.scope == Scope.GLOBAL:
            self.err(f'cannot declare local variable {tok} in global scope')
            return

        if name in self.localsym:
            e = self.localsym[name]
            if isinstance(e, PlexToken) and e.type == 'GLOBAL':
                self.err(f'cannot declare local variable to shadow explicit use of global {e}')
            else:
                self.err(f'redeclaration of local variable {tok}')
            return
        try:
            val = self.expr(expr)
        except SyntaxError as err:
            self.err(f'when set local {tok}:', err)
            return

        if self.compatible(val, (IntLit, Scalar)):
            rec = Scalar(tok)
        elif self.compatible(val, Tuple):
            rec = val
            rec.tok = tok

        self.localsym[name] = rec
        self.log(f'declared local variable {tok}')

    def defun(self, defn):
        _, tok, args, body = defn
        name = tok.value
        nargs = len(args)

        if name in self.globalsym:
            self.err(f'redeclaration of function {tok}')
            # continue anyway

        self.localsym = {}
        self.scope = Scope.LOCAL
        self.func = tok

        for arg in args:
            self.localsym[arg.value] = Scalar(tok=arg)

        # do this first, since recursion
        # TODO types aaaaaahhhhh!
        func = Func(
            nargs=nargs,
            args=args,
            rettype=Scalar,
            sym=self.localsym,
            ast=body,
            tok=tok,
        )
        self.globalsym[name] = func

        if body:
            self.compound(body)

        self.localsym = self.globalsym
        self.scope = Scope.GLOBAL
        self.func = None

        self.log(f'declared function {func}')

    def returnstmt(self, t):
        kw, e = t
        if self.scope == Scope.GLOBAL:
            self.err(f'{kw}: cannot return in global scope')
            return

        try:
            e = self.expr(e)
            self.globalsym[self.func.value].rettype = type(e)
        except SyntaxError as err:
            self.err(f'in {kw}:', err)

    def assign(self, e):
        kw, lhs, expr = e

        try:
            lhs = self.expr(lhs)
            expr = self.expr(expr)
        except SyntaxError as err:
            self.err(f'in {kw}:', err)
            return

        if not ((self.compatible(lhs, Tuple) and self.compatible(expr, Tuple) and lhs.n == expr.n) or
                (self.compatible(lhs, Scalar) and self.compatible(expr, (Scalar, IntLit)))):
            self.err(f'in {kw}: mismatched lengths')
            return

    def exchange(self, e):
        kw, lhs1, lhs2 = e

        try:
            lhs1 = self.expr(lhs1)
            lhs2 = self.expr(lhs2)
        except SyntaxError as err:
            self.err(f'in {kw}:', err)
            return

        if not ((self.compatible(lhs1, Tuple) and self.compatible(lhs2, Tuple) and lhs1.n == lhs2.n) or
                (self.compatible(lhs1, Scalar) and self.compatible(lhs2, Scalar))):
            self.err(f'{kw}: mismatched lengths')
            return

    def expr(self, e):
        # Atom
        if isinstance(e, PlexToken):
            if e.type == 'INT_LIT':
                return IntLit(e)
            if e.type == 'ID':
                return self.id(e)

        # Tuple
        if isinstance(e, list):
            fields = 0
            for ee in e:
                field = self.expr(ee)
                if self.compatible(field, (IntLit, Scalar)):
                    fields += 1
                elif self.compatible(field, Tuple):
                    fields += field.n
                else:
                    raise SyntaxError(f'invalid tuple item {field}')
            return Tuple(n=fields, ast=e, tok=None)

        t = e[0]

        # Binop
        if t in self.binop_types:
            l = self.expr(e[1])
            r = self.expr(e[2])
            if not (
                self.compatible(l, (IntLit, Scalar)) and
                self.compatible(r, (IntLit, Scalar))
            ):
                raise SyntaxError(f'invalid operands to {t} {l} {r}')
            return Scalar(tok=None)

        # TODO array index

        if t == 'INDEX':
            name = e[1].value
            d = self.symget(name)
            if not d:
                raise SyntaxError(f'not such variable {e[1]}')
            index = self.expr(e[2])
            if not self.compatible(index, (Scalar, IntLit)):
                raise SyntaxError(f'cannot reference array {d.tok} with non-integer expression')
            self.log(f'indexing array {d.tok}')
            return Scalar(tok=None)


        if t == 'TREF':
            field = e[2].value
            name = e[1].value
            d = self.symget(name)
            if not d:
                raise SyntaxError(f'no such variable {e[1]}')
            if not self.compatible(d, Tuple):
                raise SyntaxError(f'cannot reference non-tuple {d.tok}')
            if field > d.n:
                raise SyntaxError(f'cannot reference tuple {d.tok} (size {d.n}) at {field}')
            self.log(f'referencing tuple {d.tok} at {field}')
            return Scalar(tok=None)

        # Function
        if t == 'CALL':
            f = e[1]
            func = self.symget(f.value)
            if not self.compatible(func, Func):
                raise SyntaxError(f'no such function {f}')
            args = self.expr(e[2])
            if not self.compatible(args, (IntLit, Scalar, Tuple)):
                raise SyntaxError(f'cannot pass {args} to function {f}')
            self.log(f'calling function {f} with args {args}')
            return func.rettype

    def printstmt(self, t):
        kw, e = t
        try:
            e = self.expr(e)
        except SyntaxError as err:
            self.err(f'in {kw}', err)

    def id(self, t):
        name = t.value
        rec = self.symget(name)
        if not rec:
            raise SyntaxError(f'no such variable {t}')
        self.log(f'using variable {rec.tok}')
        return rec

    def compatible(self, a, b):
        if not isinstance(a, tuple):
            a = (a,)
        if not isinstance(b, tuple):
            b = (b,)

        types = []
        for typ in b:
            if not isclass(typ):
                typ = type(typ)
            types.append(typ)

        for var in a:
            if not isclass(var):
                var = type(var)
            good = False
            for typ in types:
                if var is typ:
                    good = True
            if not good:
                return False

        return True

    def err(self, *a, **kw):
        self.errors = True
        if not self.quiet:
            print('err:', *a, **kw)

    def log(self, *a, **kw):
        if self.debug and not self.quiet:
            print(*a, **kw)

class IntLit:
    def __init__(self, tok):
        self.tok = tok
    def __repr__(self):
        return f'IntLit(tok={self.tok})'

class Scalar:
    def __init__(self, tok):
        self.tok = tok
    def __repr__(self):
        return f'Scalar(tok={self.tok})'

class Tuple:
    def __init__(self, n, ast=None, tok=None):
        self.n = n
        self.ast = ast
        self.tok = tok
    def __repr__(self):
        return f'Tuple(n={self.n},ast={self.ast},tok={self.tok})'

class Array:
    def __init__(self, lo, hi, tok):
        self.lo = lo
        self.hi = hi
        self.tok = tok
    def __repr__(self):
        return f'Tuple(lo={self.lo},hi={self.hi},tok={self.tok})'

class Func:
    def __init__(self, nargs, args, rettype, sym, ast, tok):
        self.nargs = nargs
        self.args = args
        self.rettype = rettype
        self.sym = sym
        self.ast = ast
        self.tok = tok
    def __repr__(self):
        return f'Func(nargs={self.nargs},args={self.args},rettype={self.rettype},sym={self.sym},ast={self.ast},tok={self.tok})'

if __name__ == '__main__':
    f = sys.stdin if len(sys.argv)<2 else open(sys.argv[1])
    parser = parse.Pparser(f)
    ast, err = parser.parse()
    if err:
        print('ERRORS')
        sys.exit(1)
    if not ast:
        sys.exit(0)
    checker = PChecker(ast)
    checker.check()
