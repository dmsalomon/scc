
import io
import sys

import ply.yacc as yacc
from scan import Plexer

class Pparser:
    tokens = Plexer.tokens
    start = 'prog'

    def p_prog(p):
        '''
        prog : prog statement
             | prog decl
             | prog defn
        '''
        if p[1]:
            p[0] = [*p[1], p[2]]
        else:
            p[0] = [p[2]]

    def p_prog_empty(p):
        'prog : empty'

    def p_decl_array(p):
        'decl : KW_ARRAY id LBRAK range RBRAK set-expr SEMI'
        p[0] = ('ARRAY', p[2], p[4], p[6])

    def p_range(p):
        'range : expr OP_DOTDOT expr'
        p[0] = ('RANGE', p[1], p[3])

    def p_decl_set_expr(p):
        'set-expr : id ASSIGN expr'
        p[0] = ('ASSIGN', p[1], p[3])

    def p_decl_set_expr_empty(p):
        'set-expr : empty'

    def p_decl_tuple(p):
        'decl : KW_TUPLE id ASSIGN tuple-constructor SEMI'
        p[0] = ('TUPLE', p[2], p[4])

    def p_tuple_constructor(p):
        'tuple-constructor : expr OP_COMMA expr'
        if isinstance(p[1], list):
            p[0] = [*p[1], p[3]]
        else:
            p[0] = [p[1], p[3]]

    def p_decl_local(p):
        'decl : KW_LOCAL id ASSIGN expr SEMI'
        p[0] = ('LOCAL', p[2], p[4])

    def p_decl_global(p):
        'decl : KW_GLOBAL id ASSIGN expr SEMI'
        p[0] = ('GLOBAL', p[2], p[4])

    def p_func_defn(p):
        'defn : KW_DEFUN id LPAR args RPAR body KW_END KW_DEFUN'
        p[0] = ('DEFUN', p[2], p[4], p[6])

    def p_func_args_id(p):
        'args : id'
        p[0] = p[1]

    def p_func_args_list(p):
        'args : id OP_COMMA args'
        args = p[3]
        if isinstance(args, list):
            p[0] = [p[1], *args]
        else:
            p[0] = [p[1], args]

    def p_func_body(p):
        '''
        body : body statement
             | body decl
        '''
        if p[1]:
            p[0] = [*p[1], p[2]]
        else:
            p[0] = [p[2]]

    def p_func_body_empty(p):
        'body : empty'

    def p_statement_assign(p):
        'statement : lhs ASSIGN expr SEMI'
        p[0] = ('ASSIGN', p[1], p[3])

    def p_statement_exchange(p):
        'statement : lhs EXCHANGE lhs SEMI'
        p[0] = ('EXCHANGE', p[1], p[3])

    def p_statement_while(p):
        'statement : KW_WHILE bool-expr KW_DO stmts KW_END KW_WHILE'
        p[0] = ('WHILE', p[2], p[4])

    def p_statement_if(p):
        'statement : KW_IF bool-expr KW_THEN stmts elsif else KW_END KW_IF'
        p[0] = ('IF', p[2], p[4], p[5], p[6])

    def p_elsif_clause(p):
        'elsif : KW_ELSIF bool-expr KW_THEN stmts elsif'
        cur = ('ELSIF', p[2], p[4])
        elsif = p[5]
        if elsif:
            p[0] = [cur, *elsif]
        else:
            p[0] = [cur]

    def p_elsif_clause_empty(p):
        'elsif : empty'

    def p_else_clause(p):
        'else : KW_ELSE stmts'
        p[0] = ('ELSE', p[2])

    def p_else_clause_empty(p):
        'else : empty'

    def p_statement_for(p):
        'statement : KW_FOREACH id KW_IN iterable KW_DO stmts KW_END KW_FOR'
        p[0] = ('FOREACH', p[2], p[4], p[6])

    def p_iterable(p):
        '''
        iterable : range
                 | id
        '''
        p[0] = p[1]

    def p_statement_return(p):
        'statement : RETURN expr SEMI'
        p[0] = ('RETURN', p[2])

    def p_statement_print(p):
        'statement : PRINT expr SEMI'
        p[0] = ('PRINT', p[2])

    def p_stmts(p):
        'stmts : stmts statement'
        if p[1]:
            p[0] = [*p[1], p[2]]
        else:
            p[0] = [p[2]]

    def p_stmts_empty(p):
        'stmts : empty'

    def p_bool_expr(p):
        'bool-expr : expr boolop expr'
        p[0] = (p[2], p[1], p[3])

    def p_bool_op(p):
        '''
        boolop : OP_LESS
               | OP_GREATER
               | OP_EQUAL
               | OP_NOTEQUA
               | OP_LESSEQUAL
               | OP_GREATEREQUAL
        '''
        p[0] = p[1]

    def p_lhs_lvalue(p):
        'lhs : lvalue'
        p[0] = p[1]

    def p_lhs_list(p):
        'lhs : lhs OP_COMMA lvalue'
        if isinstance(p[1], list):
            p[0] = p[1] + [p[3]]
        else:
            p[0] = [p[1], p[3]]

    def p_lvalue(p):
        '''
        lvalue : id
               | tuple-ref
               | array-index
        '''
        p[0] = p[1]

    precedence = (
            ('left', 'OP_COMMA'),
            ('left', 'OP_PLUS', 'OP_MINUS'),
            ('left', 'OP_MULT', 'OP_DIV'),
            ('right', 'FUNC'),
    )

    def p_expr_tuple_constructor(p):
        'expr : tuple-constructor'
        p[0] = p[1]

    def p_expr_binop(p):
        '''
        expr : expr OP_PLUS  expr
             | expr OP_MINUS expr
             | expr OP_MULT  expr
             | expr OP_DIV   expr
        '''
        p[0] = (p[2], p[1], p[3])

    def p_expr_par(p):
        'expr : LPAR expr RPAR'
        p[0] = p[2]

    def p_expr_int(p):
        'expr : int'
        p[0] = p[1]

    def p_expr_id(p):
        'expr : id'
        p[0] = p[1]

    def p_expr_func_call(p):
        'expr : id expr %prec FUNC'
        p[0] = ('CALL', p[1], p[2])

    def p_expr_tuple_ref(p):
        'expr : tuple-ref'
        p[0] = p[1]

    def p_expr_array_index(p):
        'expr : array-index'
        p[0] = p[1]

    def p_tuple_ref(p):
        'tuple-ref : id OP_DOT int'
        p[0] = ('TREF', p[1], p[3])

    def p_array_index(p):
        'array-index : id LBRAK expr RBRAK'
        p[0] = ('INDEX', p[1], p[3])

    def p_int(p):
        'int : INT_LIT'
        p[0] = ('INT', p[1])

    def p_id(p):
        'id : ID'
        p[0] = ('ID', p[1])

    def p_empty(p):
        'empty : '
        pass

    def p_error(p):
        p.parser.error = True
        if not p:
            print("SyntaxError: unexpected EOF")
            return

        print(f'Syntax Error: unexpected token {p.type} at {p.lineno}:{p.begpos},{p.endpos}')

    def __init__(self, src):
        if isinstance(src, str):
            src = io.StringIO(src)
        self.src = src
        self.parser = yacc.yacc(module=type(self))
        self.lexer = Plexer(src, parser=self)

    def parse(self, **kw):
        self.error = False
        ast = self.parser.parse(lexer=self.lexer, **kw)
        if self.error:
            raise SyntaxError
        return ast


def pprint(o, depth=0, indent=2):
    ind = lambda: print(' '*indent*depth, end='')
    if isinstance(o, list):
        ind()
        print('[')
        for e in o:
            pprint(e, depth+1, indent)
        ind()
        print(']')
    elif isinstance(o, tuple):
        if len(o) == 1:
            pprint(o[0], depth, indent)
        elif len(o) == 2:
            ind()
            print(o[0], end='')
            print(':', end='')
            if isinstance(o[1], list) or isinstance(o[1], tuple):
                print()
                pprint(o[1], depth+1, indent)
            else:
                print(o[1])
        else:
            ind()
            print('(', end='')
            print(o[0])
            for i in range(1, len(o)):
                pprint(o[i], depth+1, indent)
            ind()
            print(')')
    else:
        ind()
        print(o)


def main():
    s = """
    x.3 = f[x],fd,((fd.3)) + fd[s], f x;
    z = (5,k);
    tr = z;
    print tr;

    while x < 4 do
        print x;
        x = x + 1;
    end while

    ***t

    if x == 4 then
        return x;
    elsif x == 5 then
        return x+4;
    elsif x >= 100 then
        return x-100;
    elsif 1 == 1,1 then
        return tr;
    elsif 10 == 10,10,10 then
    else
        return tr;
    end if

    array x[1+21..43] i=i+2;
    array x[1+21..43];

    global x = x[32+(2)];

    foreach i in x.1+1..x.2-1 do
        print i;
        print (n/i);
    end for

    defun fib(n,m,o)
        local x = 0;
        while n < 100 do
            x = x + sqrt(n-m-o);
        end while
        return x;
    end defun

    defun sin(x)
        return cos(x-pi/2);
    end defun

    *** for engineers that troll physicists
    defun sin(x)
        return x;
    end defun

    tuple tup = x[x[3]/2],1+1+1,2;

    if x,x > y,y then
        return x,x;
    end if

    defun main(argc, argv)
        foreach i in 1..argc do
            print argv[i];
        end for
        return 0;
    end defun

    local x = 7;

    print(sin(cos(x)));
    print sin cos x;
    print sin cos x-pi/2;
    print sin cos(x-pi/2);

    a = ((((b)-3*4)));
    a = ((((b)+(4*5))/6-a+b)-3);
    """
    parser = Pparser(s)

    try:
        ast = parser.parse()
    except SyntaxError:
        return 1
    print("SUCCESS")
    ast and pprint(ast, indent=2)

if __name__ == '__main__':
    sys.exit(main() or 0)
