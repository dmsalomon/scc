# \file parse.py
# \author Dov Salomon (dms833@nyu.edu)

import io
import sys

import ply.yacc as yacc
from scan import Plexer

class Pparser:
    tokens = Plexer.tokens
    start = 'prog'

    def p_prog(self, p):
        '''
        prog : prog statement
             | prog decl
             | prog defn
        '''
        p[0] = [*(p[1] or []), p[2]]

    def p_prog_error(self, p):
        'prog : prog error'
        p[0] = [*(p[1] or []), 'ERROR']

    def p_prog_empty(self, p):
        'prog : empty'

    def p_decl_array(self, p):
        'decl : KW_ARRAY id LBRAK range RBRAK set-expr SEMI'
        p[0] = ('ARRAY', p[2], p[4], p[6])

    def p_range(self, p):
        'range : expr OP_DOTDOT expr'
        p[0] = ('RANGE', p[1], p[3])

    def p_decl_set_expr(self, p):
        'set-expr : id ASSIGN expr'
        p[0] = ('ASSIGN', p[1], p[3])

    def p_decl_set_expr_empty(self, p):
        'set-expr : empty'

    def p_decl_local(self, p):
        'decl : KW_LOCAL id ASSIGN expr SEMI'
        p[0] = ('LOCAL', p[2], p[4])

    def p_decl_global(self, p):
        'decl : KW_GLOBAL id ASSIGN expr SEMI'
        p[0] = ('GLOBAL', p[2], p[4])

    def p_func_defn(self, p):
        'defn : KW_DEFUN id LPAR args RPAR body KW_END KW_DEFUN'
        p[0] = ('DEFUN', p[2], p[4], p[6])

    def p_func_args_id(self, p):
        'args : id'
        p[0] = [p[1]]

    def p_func_args_list(self, p):
        'args : args OP_COMMA id'
        p[0] = [*p[1], p[3]]

    def p_func_body(self, p):
        '''
        body : body statement
             | body decl
        '''
        p[0] = [*(p[1] or []), p[2]]

    def p_func_body_error(self, p):
        'body : body error'
        if p[1]:
            p[0] = [*p[1], 'ERROR']
        else:
            p[0] = ['ERROR']

    def p_func_body_empty(self, p):
        'body : empty'

    def p_statement_assign(self, p):
        'statement : lhs ASSIGN expr SEMI'
        p[0] = ('ASSIGN', p[1], p[3])

    def p_statement_exchange(self, p):
        'statement : lhs EXCHANGE lhs SEMI'
        p[0] = ('EXCHANGE', p[1], p[3])

    def p_statement_while(self, p):
        'statement : KW_WHILE bool-expr KW_DO stmts KW_END KW_WHILE'
        p[0] = ('WHILE', p[2], p[4])

    def p_statement_if(self, p):
        'statement : KW_IF bool-expr KW_THEN stmts elsif else KW_END KW_IF'
        p[0] = ('IF', p[2], p[4], p[5], p[6])

    def p_elsif_clause(self, p):
        'elsif : KW_ELSIF bool-expr KW_THEN stmts elsif'
        cur = ('ELSIF', p[2], p[4])
        elsif = p[5]
        if elsif:
            p[0] = [cur, *elsif]
        else:
            p[0] = [cur]

    def p_elsif_clause_empty(self, p):
        'elsif : empty'

    def p_else_clause(self, p):
        'else : KW_ELSE stmts'
        p[0] = ('ELSE', p[2])

    def p_else_clause_empty(self, p):
        'else : empty'

    def p_statement_for(self, p):
        'statement : KW_FOREACH id in iterable KW_DO stmts KW_END KW_FOR'
        p[0] = ('FOREACH', p[2], p[4], p[6])

    def p_keywork_in(self, p):
        '''
        in : KW_IN
           | ASSIGN
        '''
        p[0] = p[1]

    def p_iterable(self, p):
        '''
        iterable : range
                 | id
        '''
        p[0] = p[1]

    def p_statement_return(self, p):
        'statement : RETURN expr SEMI'
        p[0] = ('RETURN', p[2])

    def p_statement_print(self, p):
        'statement : PRINT expr SEMI'
        p[0] = ('PRINT', p[2])

    def p_stmts(self, p):
        'stmts : stmts statement'
        p[0] = [*(p[1] or []), p[2]]

    def p_stmts_error(self, p):
        'stmts : stmts error'
        p[0] = [*(p[1] or []), 'ERROR']

    def p_stmts_empty(self, p):
        'stmts : empty'

    def p_bool_expr(self, p):
        'bool-expr : expr boolop expr'
        p[0] = (p[2], p[1], p[3])

    def p_bool_op(self, p):
        '''
        boolop : OP_LESS
               | OP_GREATER
               | OP_EQUAL
               | OP_NOTEQUA
               | OP_LESSEQUAL
               | OP_GREATEREQUAL
        '''
        p[0] = p[1]

    def p_lhs_lvalue(self, p):
        'lhs : lvalue'
        p[0] = p[1]

    def p_lhs_list(self, p):
        'lhs : lhs OP_COMMA lvalue'
        if isinstance(p[1], list):
            p[0] = p[1] + [p[3]]
        else:
            p[0] = [p[1], p[3]]

    def p_lvalue(self, p):
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
            ('right', 'CALL'),
    )

    def p_expr_tuple_constructor(self, p):
        'expr : expr OP_COMMA expr'
        if isinstance(p[1], list):
            p[0] = [*p[1], p[3]]
        else:
            p[0] = [p[1], p[3]]

    def p_expr_binop(self, p):
        '''
        expr : expr OP_PLUS  expr
             | expr OP_MINUS expr
             | expr OP_MULT  expr
             | expr OP_DIV   expr
        '''
        p[0] = (p[2], p[1], p[3])

    def p_expr_par(self, p):
        'expr : LPAR expr RPAR'
        p[0] = p[2]

    def p_expr_int(self, p):
        'expr : int'
        p[0] = p[1]

    def p_expr_id(self, p):
        'expr : id'
        p[0] = p[1]

    def p_expr_func_call(self, p):
        'expr : id expr %prec CALL'
        p[0] = ('CALL', p[1], p[2])

    def p_expr_tuple_ref(self, p):
        'expr : tuple-ref'
        p[0] = p[1]

    def p_expr_array_index(self, p):
        'expr : array-index'
        p[0] = p[1]

    def p_tuple_ref(self, p):
        'tuple-ref : id OP_DOT int'
        p[0] = ('TREF', p[1], p[3])

    def p_array_index(self, p):
        'array-index : id LBRAK expr RBRAK'
        p[0] = ('INDEX', p[1], p[3])

    def p_int(self, p):
        'int : INT_LIT'
        p[0] = ('INT', p[1])

    def p_id(self, p):
        'id : ID'
        p[0] = ('ID', p[1])

    def p_empty(self, p):
        'empty : '

    def p_error(self, p):
        self.err = True
        if p:
            print(f'{p.lineno}:{p.begpos},{p.endpos}: ', end='')
        print('parser: error: ', end='')
        if p:
            print(f'unexpected `{p.value}`')
        else:
            print(f'unexpected EOF')

    def __init__(self, src):
        if isinstance(src, str):
            src = io.StringIO(src)
        self.src = src
        self.err = False

        self.lexer = Plexer(src)
        self.parser = yacc.yacc(module=self)

    def parse(self, **kw):
        self.err = False
        return self.parser.parse(lexer=self.lexer, **kw), self.err


# AST Pretty Printer
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
    f = sys.stdin if len(sys.argv)<2 else open(sys.argv[1])
    parser = Pparser(f)

    ast, err = parser.parse()
    if err:
        print("ERRORS")
    else:
        print("SUCCESS")

    pprint(ast, indent=2)

if __name__ == '__main__':
    sys.exit(main() or 0)
