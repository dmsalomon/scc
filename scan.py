
import ply.lex as lex

class PlexToken:
    def __init__(self, t):
        self.type = t.type
        if t.type != 'ID' and t.type != 'INT_LIT':
            self.text = t.value
        else:
            self.text = t.text
        self.value = t.value
        self.lineno = t.lineno
        self.begpos = t.lexpos+1
        self.endpos = self.begpos + len(self.text) - 1

    def __str__(self):
        return f'Token({self.type},{self.text},{repr(self.value)},{self.lineno},{self.begpos},{self.endpos})'

    def __repr__(self):
        return str(self)

class Plexer:
    reserved = {
        'array':    'KW_ARRAY',
        'tuple':    'KW_TUPLE',
        'local':    'KW_LOCAL',
        'global':   'KW_GLOBAL',
        'defun':    'KW_DEFUN',
        'end':      'KW_END',
        'while':    'KW_WHILE',
        'do':       'KW_DO',
        'if':       'KW_IF',
        'then':     'KW_THEN',
        'elsif':    'KW_ELSIF',
        'else':     'KW_ELSE',
        'foreach':  'KW_FOREACH',
        'in':       'KW_IN',
        'return':   'RETURN',
        'print':    'PRINT',
    }

    tokens = [
        'ID',
        'OP_DOTDOT',
        'LBRAK',
        'RBRAK',
        'SEMI',
        'LPAR',
        'RPAR',
        'OP_COMMA',
        'OP_DOT',
        'INT_LIT',
        'ASSIGN',
        'EXCHANGE',
        'OP_LESS',
        'OP_GREATER',
        'OP_LESSEQUAL',
        'OP_GREATEREQUAL',
        'OP_EQUAL',
        'OP_NOTEQUA',
        'OP_PLUS',
        'OP_MINUS',
        'OP_MULT',
        'OP_DIV',
        'OP_UMINUS',
        'COMMENT',
    ] + list(reserved.values())

    t_OP_DOTDOT         = r'\.\.'
    t_LBRAK             = r'\['
    t_RBRAK             = r'\]'
    t_SEMI              = r';'
    t_LPAR              = r'\('
    t_RPAR              = r'\)'
    t_OP_COMMA          = r','
    t_OP_DOT            = r'\.'
    t_ASSIGN            = r'='
    t_EXCHANGE          = r'<->'
    t_OP_LESS           = r'<'
    t_OP_GREATER        = r'>'
    t_OP_LESSEQUAL      = r'<='
    t_OP_GREATEREQUAL   = r'>='
    t_OP_EQUAL          = r'=='
    t_OP_NOTEQUA        = r'!='
    t_OP_PLUS           = r'\+'
    t_OP_MULT           = r'\*'
    t_OP_DIV            = r'\/'
    t_COMMENT           = r'\*\*\*.*$'

    t_ignore            = ' \t\v\r'

    def t_ID(self, t):
        r'[_a-zA-Z]+'
        t.type = self.reserved.get(t.value, t.type)
        t.text = t.value
        if len(t.value) > 31:
            print(f'warning: identifier too long: {t.value} truncated to {t.value[:31]}')
            t.value = t.value[:31]
        return t

    def t_INT_LIT(self, t):
        r'\d+'
        INT32_MAX = 0x7fffffff
        t.text = t.value
        t.value = int(t.value)
        if t.value > INT32_MAX:
            v = t.value
            t.value &= INT32_MAX
            print(f"warning: integer larger than 32-bits: {v} truncated to {t.value}")
        return t

    def t_OP_UMINUS(self, t):
        r'-'
        if self.prev and self.prev.type in (
                'RPAR', 'ID', 'INT_LIT', 'RBRAK'):
            t.type = 'OP_MINUS'
        return t

    def t_newline(self, t):
        r'\n+'
        self.lineno += len(t.value)

    def t_error(self, t):
        t = self.rectify_token(t)
        print(f"error: '{t.value[0]}' found at {t.lineno}: {t.begpos},{t.endpos}")
        self.lexer.skip(1)

    def t_eof(self, t):
        more = self.src.readline()
        if more != '':
            self.lexer.input(more)
            return self.lexer.token()

    def __init__(self, src, **kw):
        self.lexer = lex.lex(object=self, **kw)
        self.src = src
        self.lineno = 1
        self.tok = None
        self.prev = None

    def rectify_token(self, t):
        if t:
            t.lineno = self.lineno
            return PlexToken(t)
        return t

    def ntok(self):
        self.prev = self.rectify_token(self.lexer.token())
        return self.prev

    def next(self):
        if self.tok:
            t, self.tok = self.tok, None
            return t
        return self.ntok()

    token = next

    def peek(self):
        if not self.tok:
            self.tok = self.ntok()
        return self.tok

    def __iter__(self):
        while self.peek():
            yield self.next()

from sys import argv, stdin

f = stdin if len(argv) < 2 else open(argv[1], 'r')
m = Plexer(f)

for tok in m:
    print(tok)
