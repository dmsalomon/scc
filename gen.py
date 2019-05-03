
from check import *
from llvmlite import ir, binding

binding.initialize()
binding.initialize_native_target()
binding.initialize_native_asmprinter()

i32 = ir.IntType(32)
i32p = i32.as_pointer()
charptr = ir.IntType(8).as_pointer()

def structy(n):
    return ir.LiteralStructType([i32]*n)

class PGen:
    def __init__(self, checker):
        self.c = checker

        mod = ir.Module(name=__file__)
        mod.triple = binding.get_default_triple()
        target = binding.Target.from_default_triple()
        target_machine = target.create_target_machine()
        backing = binding.parse_assembly('')
        engine = binding.create_mcjit_compiler(backing, target_machine)

        self.mod = mod
        self.engine = engine

        self.gx = {}
        self.lx = {}

        self.globals(self.c.sym)
        self.builtins()
        self.functions()
        self.main()

    def globals(self, sym):
        gx = self.gx

        for name, typ in sym.items():
            if self.c.compatible(typ, Func):
                continue

            if self.c.compatible(typ, Scalar):
                t = i32
            elif self.c.compatible(typ, Tuple):
                n = typ.n
                t = structy(n)
            elif self.c.compatible(typ, Array):
                sz = typ.hi - typ.lo + 1
                assert(sz > 0)
                t = ir.ArrayType(i32, sz)
            else:
                raise Exception(typ)

            g = ir.GlobalVariable(self.mod, t, name=name)
            g.linkage = 'internal'
            gx[name] = g

    def builtins(self):
        func_type = ir.FunctionType(i32, [charptr], var_arg=True)
        printf= ir.Function(self.mod, func_type, name='printf')
        scanf = ir.Function(self.mod, func_type, name='scanf')
        self.printf = printf
        self.scanf = scanf

        func_type = ir.FunctionType(ir.VoidType(), [i32], var_arg=False)
        pint = ir.Function(self.mod, func_type, name='pint')
        builder = ir.IRBuilder(pint.append_basic_block(name='entry'))
        fmt = '%d\n\0'
        g_fmt = self.conststr(fmt)
        fmt_arg = builder.bitcast(g_fmt, charptr)
        n = pint.args[0]
        builder.call(printf, [fmt_arg, n])
        builder.ret_void()
        self.pint = pint

        fty = ir.FunctionType(i32, [], False)
        main = ir.Function(self.mod, fty, name='main')
        self.entry = main

        self.fx = [printf, scanf, pint, main]

    def functions(self):
        for name,f in self.c.sym.items():
            if not self.c.compatible(f, Func):
                continue

            nargs = f.arg.n
            assert(nargs > 0)
            ret = f.rettype

            arg = structy(nargs)

            if self.c.compatible(ret, Scalar):
                ret = i32
            else:
                assert(ret.n > 0)
                ret = structy(ret.n)

            f_ty = ir.FunctionType(ret, [arg], var_arg=False)
            func = ir.Function(self.mod, f_ty, name)
            self.fx.append(func)

    def main(self):
        entryblock = self.entry.append_basic_block(name='entry')
        self.builder = ir.IRBuilder(entryblock)
        self.sym = self.c.sym
        self.compound(self.c.ast)
        self.builder.ret(i32(0))

    def compound(self, ast):
        switch = {
            'GLOBAL': self.globaldecl,
            'ARRAY': self.arraydecl,
            'ASSIGN': self.assign,
            'EXCHANGE': self.exchange,
            'DEFUN': lambda s: None,
            'PRINT': self.printstmt,
            'IF': self.ifstmt,
            'WHILE': self.whilestmt,
            'FOREACH': self.foreachstmt,
        }
        for s in ast:
            t = s[0].type
            f = switch.get(t)
            if f: f(s)

    def ifstmt(self, s):
        _, b, sx, ei, els = s

        cond = self._bool(b)

        blockif = self.builder.append_basic_block()
        blockel = self.builder.append_basic_block()
        blockjoin = self.builder.append_basic_block()

        self.builder.cbranch(cond, blockif, blockel)

        self.builder = ir.IRBuilder(blockif)
        self.compound(sx)
        self.builder.branch(blockjoin)

        self.builder = ir.IRBuilder(blockel)

        if ei:
            for _, b, sx in ei:
                blockif = self.builder.append_basic_block()
                blockel = self.builder.append_basic_block()
                cond = self._bool(b)
                self.builder.cbranch(cond, blockif, blockel)

                self.builder = ir.IRBuilder(blockif)
                self.compound(sx)
                self.builder.branch(blockjoin)
                self.builder = ir.IRBuilder(blockel)

        if els:
            _, sx = els
            self.compound(sx)

        self.builder.branch(blockjoin)

        self.builder = ir.IRBuilder(blockjoin)

    def whilestmt(self, s):
        _, b, sx = s

        blockcond = self.builder.append_basic_block()
        blockbody = self.builder.append_basic_block()
        blockend = self.builder.append_basic_block()

        self.builder.branch(blockcond)

        self.builder = ir.IRBuilder(blockcond)
        cond = self._bool(b)
        self.builder.cbranch(cond, blockbody, blockend)

        self.builder = ir.IRBuilder(blockbody)
        self.compound(sx)
        self.builder.branch(blockcond)

        self.builder = ir.IRBuilder(blockend)

    def foreachstmt(self, s):
        _, i, c, sx = s

        lo, hi = self.range(c)
        i = self.expr(i)
        iback = self.builder.alloca(i32)
        self.builder.store(lo, iback)

        blockcond = self.builder.append_basic_block()
        blockbody = self.builder.append_basic_block()
        blockend = self.builder.append_basic_block()

        self.builder.branch(blockcond)

        self.builder = ir.IRBuilder(blockcond)
        inext = self.builder.load(iback)
        self.builder.store(inext, i)
        cond = self.builder.icmp_signed('>', inext, hi)
        self.builder.cbranch(cond, blockend, blockbody)

        self.builder = ir.IRBuilder(blockbody)
        self.compound(sx)
        inext = self.builder.add(inext, i32(1))
        self.builder.store(inext, iback)
        self.builder.branch(blockcond)

        self.builder = ir.IRBuilder(blockend)


    def _bool(self, b):
        op, lhs, rhs = b
        lhs = self.load(lhs)
        rhs = self.load(rhs)
        return self.builder.icmp_signed(op, lhs, rhs)

    def printstmt(self, s):
        _, e = s
        e = self.load(e)
        self.builder.call(self.pint, [e])

    def globaldecl(self, decl):
        _, tok, expr = decl

        if not expr:
            return

        v = self.load(expr)
        g = self.gx[tok.value]
        self.builder.store(v, g)

    def arraydecl(self, decl):
        _, arr, r, se = decl

        if not se:
            return

        _, i, expr = se
        arr = self.expr(arr)

        lo, hi = self.range(r)
        i = self.expr(i)
        self.builder.store(lo, i)

        blockcond = self.builder.append_basic_block()
        blockbody = self.builder.append_basic_block()
        blockend = self.builder.append_basic_block()

        self.builder.branch(blockcond)

        self.builder = ir.IRBuilder(blockcond)
        ival = self.builder.load(i)
        cond = self.builder.icmp_signed('>', ival, hi)
        self.builder.cbranch(cond, blockend, blockbody)

        self.builder = ir.IRBuilder(blockbody)
        ai = self.load(expr)
        ioffset = self.builder.sub(ival, lo)
        ap = self.builder.gep(arr, [i32(0), ioffset])
        self.builder.store(ai, ap)
        ival = self.builder.load(i)
        ival = self.builder.add(ival, i32(1))
        self.builder.store(ival, i)
        self.builder.branch(blockcond)

        self.builder = ir.IRBuilder(blockend)

    def assign(self, s):
        _, lhs, rhs = s

        loc = self.expr(lhs)
        val = self.load(rhs)
        self.builder.store(val, loc)

    def exchange(self, s):
        _, lhs, rhs = s

        lloc = self.expr(lhs)
        lval = self.load(lhs)
        rloc = self.expr(rhs)
        rval = self.load(rhs)
        self.builder.store(rval, lloc)
        self.builder.store(lval, rloc)

    def range(self, r):
        _, el, er = r
        vl = self.load(el)
        vr = self.load(er)
        return vl, vr

    def load(self, e):
        e = self.expr(e)
        if e.type == i32:
            return e
        if e.type == i32p:
            return self.builder.load(e)
        raise SyntaxError

    def expr(self, e):
        # Atom
        if isinstance(e, PlexToken):
            if e.type == 'INT_LIT':
                return i32(e.value)
            if e.type == 'ID':
                return self.id(e)

        t = e[0]

        if t in self.c.binop_types:
            _, lhs, rhs = e

            lhs = self.load(lhs)
            rhs = self.load(rhs)

            if t == '+':
                return self.builder.add(lhs, rhs)
            if t == '-':
                return self.builder.sub(lhs, rhs)
            if t == '*':
                return self.builder.mul(lhs, rhs)
            if t == '/':
                return self.builder.sdiv(lhs, rhs)

        if t == 'INDEX':
            _, arr, index = e
            rec = self.c.symget(arr.value)
            lo = rec.lo

            arr = self.expr(arr)
            index = self.load(index)

            if lo:
                index = self.builder.sub(index, i32(lo))

            return self.builder.gep(arr, [i32(0), index])

    def id(self, t):
        if self.c.compatible(t, PlexToken):
            name = t.value
        else:
            name = t
        if name in self.lx:
            return self.lx[name]
        if name in self.gx:
            return self.gx[name]
        raise Exception()

    def conststr(self, s):
        string_ty = ir.ArrayType(ir.IntType(8), len(s))
        c_fmt = ir.Constant(string_ty, bytearray(s.encode('utf-8')))
        name = self.mod.get_unique_name()
        g_fmt = ir.GlobalVariable(self.mod, c_fmt.type, name=name)
        g_fmt.linkage = 'internal'
        g_fmt.global_constant = True
        g_fmt.initializer = c_fmt
        self.gx[name] = g_fmt
        return g_fmt

    def compile(self):
        llvm_ir = str(self.mod)
        with open('o.llx', 'w') as f:
            print(llvm_ir, file=f)
        try:
            mod = binding.parse_assembly(llvm_ir)
        except Exception as e:
            print(llvm_ir)
            raise(e)
        mod.verify()
        self.engine.add_module(mod)
        self.engine.finalize_object()
        self.engine.run_static_constructors()
        with open('o.ll', 'w') as f:
            print(mod, file=f)

if __name__ == '__main__':
    f = sys.stdin if len(sys.argv)<2 else open(sys.argv[1])
    parser = parse.Pparser(f)
    ast, err = parser.parse()
    if err:
        print('ERRORS')
        sys.exit(1)
    if not ast:
        sys.exit(0)
    checker = PChecker(ast, debug=False)
    checker.check()
    if checker.errors:
        raise SyntaxError
    codegen = PGen(checker)
    codegen.compile()
