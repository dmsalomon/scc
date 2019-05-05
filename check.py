
import sys

from copy import copy, deepcopy
from enum import Enum, auto
from inspect import isclass

import parse
from scan import PlexToken

class Scope(Enum):
    GLOBAL = auto()
    LOCAL = auto()

class PChecker:
    binop_types = ('+', '-', '*', '/', '<', '>', '==', '!=', '<=', '>=')

    def __init__(self, ast, debug=True):
        self.ast = ast
        self.debug = debug
        self.globalsym = {}
        self.localsym = self.globalsym
        self.sym = self.globalsym
        self.scope = Scope.GLOBAL
        self.func = None
        self.errors = False

    def check(self):
        for n, f in builtins.items():
            self.globalsym[n] = f

        self.compound(self.ast)

        self.log('symbol table:', self.globalsym)

        # TODO check for lingering amibguities
        for n,r in self.globalsym.items():
            if self.compatible(r, Undefined):
                self.err(f'{r.tok} remains undefined')
            elif self.compatible(r, Tuple) and r.n < 0:
                self.err(f'size of {r.tok} remains unknown')
            elif self.compatible(r, Func):
                if r.arg.n < 0:
                    self.err(f'number of arguments to {r.tok} remains unknown')
                if self.compatible(r.rettype, Fret):
                    self.err(f'return type of {r.tok} remains unknown')
                if self.compatible(r.rettype, Tuple) and r.rettype.n < 0:
                    self.err(f'number of elements in return for {r.tok} remains unknown')


    def compound(self, ast):
        switch = {
            'ARRAY': self.arraydecl,
            'GLOBAL': self.globaldecl,
            'LOCAL': self.localdecl,
            'DEFUN': self.defun,
            'PRINT': self.printstmt,
            'RETURN': self.returnstmt,
            'ASSIGN': self.assign,
            'EXCHANGE': self.assign,
            'IF': self.ifstmt,
            'WHILE': self.whilestmt,
            'FOREACH': self.foreachstmt,
        }
        for s in ast:
            t = s[0].type
            f = switch.get(t, lambda s: self.log("WTF:", s))
            f(s)

    def arraydecl(self, decl):
        _, tok, r, se = decl
        name = tok.value
        arr = self.localsym

        try:
            rec = self.symget(name)
            if not self.compatible(rec, Uninitialized):
                raise SyntaxError(f'reinitialization of {name}')
        except SyntaxError as e:
            self.err(f'in array initiliazation: {e}')

        try:
            lo, hi = self.arrayrange(r)
        except SyntaxError as err:
            self.err(f'when parsing range for {tok}:', err)
            return

        if se:
            _, itok, e = se
            name = itok.value
            if name in self.localsym:
                self.err(f'set-expr for {tok}: {itok} already in use')
            else:
                ind = Scalar(tok=itok)
                self.localsym[name] = ind
                e = self.expr(e)
                if not self.compatible(e, (IntLit, Scalar)):
                    self.err(f'set-expr for {tok}: expression must be int')

        a = Array(lo=lo, hi=hi, tok=tok)
        self.localsym[tok.value] = a
        self.log(f'declaring array {a}')

    def arrayrange(self, r):
        _, e1, e2 = r
        t1 = self.expr(e1)
        t2 = self.expr(e2)
        if not (self.compatible(t1, IntLit) and self.compatible(t2, IntLit)):
            raise SyntaxError('range endpoints must be constants')
        lo = t1.tok.value
        hi = t2.tok.value

        if lo > hi:
            raise SyntaxError('lo > hi')

        return lo,hi

    def looprange(self, r):
        _, e1, e2 = r
        t1 = self.expr(e1)
        t2 = self.expr(e2)
        if not self.compatible((t1, t2), (IntLit, Scalar)):
            raise SyntaxError('range endpoints must be int')

    def globaldecl(self, decl):
        kw, tok, expr = decl
        name = tok.value

        if self.scope == Scope.GLOBAL:
            if name in self.globalsym:
                self.err(f'redeclaration of {tok}')
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
                g = self.globalsym[name]
                if self.compatible(g, Func):
                    self.err('cannot use function name as variable')
                    return
                self.localsym[name] = g
                self.log(f'using global variable {tok} in {self.func.tok}')
            return

        if not expr:
            val = Uninitialized(tok=tok)
        else:
            try:
                val = self.expr(expr)
            except SyntaxError as err:
                self.err(f'when set global {tok}:', err)
                return

        if self.compatible(val, (IntLit, Scalar)):
            rec = Scalar(tok)
        elif self.compatible(val, Tuple):
            if val.n < 0:
                rec = val.link()
            else:
                rec = deepcopy(val)
                rec.dep = val.dep
            rec.tok = tok
        else:
            rec = val

        self.localsym[name] = rec
        self.log(f'declared global var {tok}')

    def localdecl(self, decl):
        _, tok, expr = decl
        name = tok.value

        if self.scope == Scope.GLOBAL:
            self.err(f'cannot declare local variable {tok} in global scope')
            return

        if name in self.localsym:
            self.err(f'redeclaration of {tok}')
            return


        if not expr:
            val = Uninitialized(tok=tok)
        else:
            try:
                val = self.expr(expr)
            except SyntaxError as err:
                self.err(f'when setting local {tok}:', err)
                return

        if self.compatible(val, (IntLit, Scalar)):
            rec = Scalar(tok)
        elif self.compatible(val, Tuple):
            if val.n < 0:
                rec = val.link()
            else:
                rec = deepcopy(val)
                rec.dep = val.dep
            rec.tok = tok
        else:
            rec = val

        self.localsym[name] = rec
        self.log(f'declared local variable {tok}')

    def defun(self, defn):
        _, tok, args, body = defn
        name = tok.value
        nargs = len(args)

        if nargs > 1:
            self.err('implementation restriction: functions can only have one formal parameter')
            return

        if name in self.globalsym:
            if self.isreserved(name):
                self.err(f'in {tok}: redeclaration of builtin {name}')
                return
            else:
                self.err(f'redeclaration of function {tok}')
            # continue anyway

        self.localsym = {}
        self.scope = Scope.LOCAL

        arg = args[0]
        arg = Tuple(tok=arg, n=-1)
        self.localsym[arg.tok.value] = arg

        self.func = Func(
            arg=arg,
            sym=self.localsym,
            ast=body,
            tok=tok,
        )
        self.func.rettype = Fret(f=self.func)
        self.globalsym[name] = self.func

        if body:
            self.compound(body)

        self.localsym = self.globalsym
        self.scope = Scope.GLOBAL

        if not self.func.didret:
            self.err(f'{tok} did not return')

        self.log(f'declared function {self.func}')
        self.func = None


    def returnstmt(self, t):
        kw, e = t
        if self.scope == Scope.GLOBAL:
            self.err(f'{kw}: cannot return in global scope')
            return

        rettype = self.func.rettype

        try:
            e = self.expr(e)
            if self.compatible(e, Array):
                self.err(f'{kw}: cannot return array {e.tok} from function')
                return

            if self.compatible(rettype, Fret):
                if self.compatible(e, Tuple) and e.n < 0:
                    e = e.link()
                rettype = rettype.resolve(e)

            if (not self.compatible(e, rettype) or
                    self.compatible(e, Tuple) and rettype.n > 0 and e.n != rettype.n):
                    if not self.compatible((e, rettype), (IntLit, Scalar)):
                        self.err(f'{kw}: previously returned {rettype}')

        except SyntaxError as err:
            self.err(f'in {kw}:', err)

        self.func.didret = True

    def assign(self, e):
        kw, lhs, rhs = e

        try:
            lhs = self.expr(lhs)
            rhs = self.expr(rhs)
        except SyntaxError as err:
            self.err(f'in {kw}:', err)
            return

        if self.compatible(lhs, Array):
            self.err(f'in {kw}: cannot assign to bare array')
            return

        if self.compatible(rhs, Array):
            self.err(f'in {kw}: cannot assign from bare array')
            return

        if self.compatible(lhs, Scalar) and not self.compatible(rhs, (Scalar, IntLit)):
            self.err(f'in {kw}: assigning non-int to int')

        if self.compatible(lhs, Tuple):
            if not self.compatible(rhs, Tuple):
                self.err(f'in {kw}: assigning non-tuple to tuple')
            elif lhs.n < 0 and rhs.n > 0:
                self.log(f'in {kw}: infering size of {lhs.tok} to be {rhs.n}')
                lhs.resolve(rhs.n)
            elif lhs.n > 0 and rhs.n < 0:
                self.log(f'in {kw}: infering size of {rhs.tok} to be {lhs.n}')
                rhs.resolve(lhs.n)
            elif lhs.n != rhs.n:
                self.err(f'in {kw}: mismatched lengths')

        if self.compatible(lhs, Fret) and not self.compatible(rhs, Fret):
            lhs = lhs.resolve(rhs)
        elif not self.compatible(lhs, Fret) and self.compatible(rhs, Fret):
            if lhs.tok:
                rhs.link(self.localsym, lhs.tok)

    def ifstmt(self, s):
        kw, b, sx, ei, els = s

        try:
            self.expr(b)
            if sx:
                self.compound(sx)
        except SyntaxError as e:
            self.err(f'in {kw}: ', e)

        if ei:
            self.elsif(ei)

        if els:
            kw, sx = els
            try:
                self.compound(sx)
            except SyntaxError as e:
                self.err(f'in {kw}: ', e)

    def elsif(self, s):
        for clause in s:
            kw, b, sx = clause
            try:
                self.expr(b)
                if sx:
                    self.compound(sx)
            except SyntaxError as e:
                self.err(f'in {kw}: ', e)

    def whilestmt(self, s):
        kw, b, sx = s

        try:
            self.expr(b)
            if sx:
                self.compound(sx)
        except SyntaxError as e:
            self.err(f'in {kw}: ', e)

    def foreachstmt(self, s):
        kw, i, c, sx = s

        try:
            if i.value in self.localsym:
                raise SyntaxError(f'cannot declare iterator {i}: name already in use')
            self.symput(i.value, Scalar(tok=i))
            self.iterable(c)
            if sx:
                self.compound(sx)
        except SyntaxError as e:
            self.err(f'in {kw}: ', e)

        self.log(f'for loop {kw}: iterator {i}: iterable {c}')

    def iterable(self, s):
        if self.compatible(s, PlexToken):
            d = self.id(s)
            if not self.compatible(d, Array):
                raise SyntaxError(f'cannot iterate over non array variable {d}')
        else:
            self.looprange(s)

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
                    if field.n < 0:
                        fields = field.n
                        break
                    fields += field.n
                elif self.compatible(field, Fret):
                    fields = -1
                    break
                else:
                    raise SyntaxError(f'invalid tuple item {field}')
            return Tuple(n=fields, ast=e, tok=None)

        t = e[0]

        # Binop
        if t in self.binop_types:
            l = self.expr(e[1])
            r = self.expr(e[2])

            if self.compatible(l, Fret):
                l = l.resolve(Scalar())

            if self.compatible(r, Fret):
                r = r.resolve(Scalar())

            if not (
                self.compatible(l, (IntLit, Scalar)) and
                self.compatible(r, (IntLit, Scalar))
            ):
                raise SyntaxError(f'invalid operands to {t}: {l} & {r}')
            return Scalar(tok=None)

        # TODO array index

        if t == 'INDEX':
            _, name, index = e
            index = self.expr(index)

            d = self.id(name)
            if not self.compatible(d, Array):
                raise(f'{d.tok} is not array: cannot be indexed')

            if self.compatible(index, Fret):
                index = index.resolve(Scalar())

            if not self.compatible(index, (Scalar, IntLit)):
                raise(f'cannot reference array {d.tok} with non-integer expression')

            self.log(f'indexing array {d.tok}')
            return Scalar(tok=None)


        if t == 'TREF':
            field = e[2].value
            name = e[1].value

            d = self.symget(name)
            if not self.compatible(d, Tuple):
                raise SyntaxError(f'cannot reference non-tuple {d.tok}')

            if d.n < 0:
                d.resolve(-field)

            if d.n > 0 and field > d.n:
                raise SyntaxError(f'cannot reference tuple {d.tok} (size {d.n}) at {field}')
            if field == 0:
                raise SyntaxError(f'cannot reference tuple {d.tok} at field 0')
            self.log(f'referencing tuple {d.tok} at {field}')
            return Scalar(tok=None)

        # Function
        if t == 'CALL':
            f = e[1]

            try:
                func = self.symget(f.value)
            except:
                self.err(f'no such function {f}')
                return

            if not self.compatible(func, Func):
                raise SyntaxError(f'no such function {f}')

            args = self.expr(e[2])
            if not self.compatible(args, (IntLit, Scalar, Tuple)):
                raise SyntaxError(f'cannot pass {args} to function {f}')

            if func.arg.n > 0:
                passed = 1
                if self.compatible(args, Tuple):
                    passed = args.n
                    if passed < 0 and args.tok:
                        args.resolve(func.arg.n)
                if passed > 0 and passed != func.arg.n:
                    raise SyntaxError(f'pass incorrect number of args to {f}')
            else:
                passed = 1
                if self.compatible(args, Tuple):
                    passed = args.n
                if passed > 0:
                    if passed < -func.arg.n:
                        raise SyntaxError(f'passing too few args to {f}')
                    else:
                        func.arg.resolve(passed)
                else:
                    if len(func.arg.dep) < 2:
                        func.arg = args.link()

            nargs = f"({'ambiguous' if passed < 0 else passed})"
            self.log(f'calling function {func.tok} with {nargs} args {args}')
            return func.rettype

    def printstmt(self, t):
        kw, e = t
        try:
            e = self.expr(e)
            if self.compatible(e, Fret):
                e = e.resolve(Scalar())
            if not self.compatible(e, (IntLit, Scalar)):
                raise SyntaxError('cannot print not integer expression')
        except SyntaxError as err:
            self.err(f'in {kw}', err)

    def id(self, t):
        if self.compatible(t, PlexToken):
            name = t.value
        elif self.compatible(t, str):
            name = t
        else:
            raise Exception
        rec = self.localsym.get(name)
        if not rec:
            raise SyntaxError(f'no such variable {t}')
        if self.compatible(rec, Uninitialized):
            raise SyntaxError(f'using uninitialized variable {t}')
        if self.compatible(t, PlexToken):
            self.log(f'using variable {t} declared at {rec.tok}')
        else:
            self.log(f'using variable {rec.tok}')
        return rec

    def symget(self, name):
        if name in self.localsym:
            return self.localsym[name]
        if name in self.globalsym:
            return self.globalsym[name]
        raise SyntaxError(f'no such variable {name}')

    def symput(self, name, rec):
        if self.scope == Scope.GLOBAL:
            self.globalsym[name] = rec
        else:
            if (name in self.localsym and
                name in self.globalsym and
                self.localsym[name] is self.globalsym[name]):
                    self.globalsym[name] = rec
            self.localsym[name] = rec

    def isreserved(self, name):
        return (name in self.globalsym) and (self.globalsym[name] is Reserved)

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
        print('err:', *a, **kw)

    def log(self, *a, **kw):
        if self.debug:
            print(*a, **kw)

class IntLit:
    def __init__(self, tok):
        self.tok = tok
    def __repr__(self):
        return f'IntLit(tok={self.tok})'

class Undefined:
    def __init__(self, tok=None):
        self.tok = tok
    def __repr__(self):
        return f'Undefined(tok={self.tok})'

class Fret:
    def __init__(self, f):
        self.f = f
        self.dep = [self]
        self.dec = False

    def link(self, sym, tok):
        fr = Fret(f=self.f)
        fr.dec = True
        fr.dep = self.dep
        self.sym = sym
        self.tok = tok
        self.dep.append(fr)

    def __repr__(self):
        return f'Fret(f={self.f.tok})'

    def resolve(self, rec):
        mine = None
        for fr in self.dep:
            ret = fr.establish(rec)
            if fr is self:
                mine = ret
        return mine

    def establish(self, rec):
        if self.dec:
            name = tok.value
            rec.tok.value = name
            sym[name] = rec
        else:
            rec.tok = None
            self.f.rettype = rec
        return rec

class Uninitialized:
    def __init__(self, tok=None):
        self.tok = tok
    def __repr__(self):
        return f'Uninitialized(tok={self.tok})'

class Scalar:
    def __init__(self, tok=None):
        self.tok = tok
    def __repr__(self):
        return f'Scalar(tok={self.tok})'

class Tuple:
    def __init__(self, n, ast=None, tok=None):
        self.n = n
        self.ast = ast
        self.tok = tok
        self.dep = [self]

    def link(self):
        t = Tuple(n=self.n,tok=None)
        t.dep = self.dep
        self.dep.append(t)
        return t

    def resolve(self, n):
        for t in self.dep:
            t.n = n

    def __repr__(self):
        nlinks = len(self.dep)
        nlinks = nlinks if nlinks > 1 else ""
        return f'Tuple(tok={self.tok},n={self.n},links={nlinks})'

class Array:
    def __init__(self, lo, hi, tok):
        self.lo = lo
        self.hi = hi
        self.tok = tok
    def __repr__(self):
        return f'Array(tok={self.tok},lo={self.lo},hi={self.hi})'

class Func:
    def __init__(self, arg, sym, ast, tok, rettype=None, didret=False):
        self.arg = arg
        self.sym = sym
        self.ast = ast
        self.tok = tok
        self.rettype = rettype
        self.didret = False
    def __repr__(self):
        return f'Func(tok={self.tok},arg={self.arg},rettype={self.rettype},sym={self.sym})'

Reserved = ()
s = Scalar()
builtins = {
    'input': Func(
        arg=Tuple(n=1),
        sym=None,
        ast=None,
        tok='(input: builtin)',
        rettype=s,
        didret=True,
    ),
}

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
