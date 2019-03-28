
import ply.yacc as yacc
import sys

from scan import Plexer


class Pparser:
    tokens = Plexer.tokens

    def p_prog(p):
        '''
        prog : statement prog
             | decl prog
             | defn prog
             | empty
        '''
        pass

    def p_decl_array(p):
        'decl : KW_ARRAY ID LBRAK expr OP_DOTDOT expr RBRAK set-expr SEMI'
        pass

    def p_decl_set_expr(p):
        '''
        set-expr : ASSIGN expr
                 | empty
        '''
        pass

    def p_decl_tuple(p):
        'decl : KW_TUPLE ID ASSIGN tuple-constructor SEMI'
        pass

    def p_tuple_constructor(p):
        '''
        tuple-constructor : expr OP_COMMA expr
        '''
        print('tuple-constructor', p.lineno(2), p.lexpos(2))
        pass

    def p_decl_var(p):
        '''
        decl : KW_LOCAL  ID ASSIGN expr SEMI
             | KW_GLOBAL ID ASSIGN expr SEMI
        '''
        pass

    def p_func_defn(p):
        'defn : KW_DEFUN ID LPAR args RPAR body KW_END KW_DEFUN'
        pass

    def p_func_args(p):
        '''
        args : ID
             | args OP_COMMA ID
        '''

    def p_func_body(p):
        '''
        body : statement body
             | decl body
             | empty
        '''
        pass

    def p_statement_assign(p):
        'statement : lhs ASSIGN expr SEMI'
        pass

    def p_statement_exchange(p):
        'statement : lhs EXCHANGE lhs SEMI'
        pass

    def p_statement_while(p):
        'statement : KW_WHILE bool-expr KW_DO stmts KW_END KW_WHILE'
        pass

    def p_statement_if(p):
        'statement : KW_IF bool-expr KW_THEN stmts elsif else KW_END KW_IF'
        pass

    def p_elsif_clause(p):
        '''
        elsif : KW_ELSIF bool-expr KW_THEN stmts elsif
              | empty
        '''
        pass

    def p_else_clause(p):
        '''
        else : KW_ELSE stmts
             | empty
        '''
        pass

    def p_statement_for(p):
        'statement : KW_FOREACH ID KW_IN ID KW_DO stmts KW_END KW_FOR'
        pass

    def p_statement_return(p):
        'statement : RETURN expr SEMI'

    def p_statement_print(p):
        'statement : PRINT expr SEMI'
        pass

    def p_stmts(p):
        '''
        stmts : statement stmts
              | empty
        '''

    def p_bool_expr(p):
        '''
        bool-expr : expr OP_LESS         expr
                  | expr OP_GREATER      expr
                  | expr OP_EQUAL        expr
                  | expr OP_NOTEQUA      expr
                  | expr OP_LESSEQUAL    expr
                  | expr OP_GREATEREQUAL expr
        '''
        pass

    def p_lhs(p):
        '''
        lhs : lvalue
            | lhs OP_COMMA lvalue
        '''
        pass

    def p_lvalue(p):
        '''
        lvalue : ID
               | ID OP_DOT INT_LIT
               | ID LBRAK expr RBRAK
        '''
        pass

    precedence = (
            ('left', 'OP_COMMA'),
            ('left', 'OP_PLUS', 'OP_MINUS'),
            ('left', 'OP_MULT', 'OP_DIV'),
    )

    def p_expr_tuple_constructor(p):
        'expr : tuple-constructor'
        pass

    def p_expr_binop(p):
        '''
        expr : expr OP_PLUS  expr
             | expr OP_MINUS expr
             | expr OP_MULT  expr
             | expr OP_DIV   expr
        '''
        pass

    def p_expr_par(p):
        'expr : LPAR expr RPAR'
        p[0] = p[2]

    def p_expr_unit(p):
        'expr : INT_LIT'
        p[0] = p[1]

    def p_expr_func_call(p):
        'expr : ID LPAR expr RPAR'
        pass

    def p_expr_lvalue(p):
        'expr : lvalue'
        pass

    def p_empty(p):
        'empty : '
        pass

    def p_error(p):
        print(p)

    def __init__(self, src):
        self.src = src
        self.lexer = Plexer(src)
        self.parser = yacc.yacc(module=type(self))

    def parse(self):
        return self.parser.parse(lexer=self.lexer)


def main():
    import io
    s = """
    x.3 = f[x],fd,((fd.3)) + fd[s], f(x);
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
    else
        return tr;
    end if

    array x[21..43] = 4;

    global x = x[32+(2)];

    defun fib(n,m,o)
        local x = 0;
        while n < 100 do
            x = x + sqrt(n-m-o);
        end while
        return x;
    end defun

    tuple tup = 1+1+1,2;

    if x,x > y,y then
        return x,x;
    end if
    """
    n = Pparser(io.StringIO(s))
    print(s, '.')
    z = n.parse()

if __name__ == '__main__':
    main()
