# \file scan.py
# \author Dov Salomon (dms833@nyu.edu)

import ply.lex as lex
import sys

class PlexToken:
    def __init__(self, _type, text, value, lineno, begpos, endpos, lexer=None):
        self.type   = _type
        self.text   = text
        self.value  = value
        self.lineno = lineno
        self.begpos = begpos
        self.endpos = endpos
        self.lexpos = begpos
        self.lexer = lexer
        if lexer:
            self.parser = lexer.parser
        else:
            self.parser = None

    @classmethod
    def from_token(cls, t):
        text = t.text if t.type in ('ID', 'INT_LIT') else t.value
        return cls(
            t.type,
            text,
            t.value,
            t.lineno,
            t.lexpos+1,
            t.lexpos + len(text),
            t.lexer,
        )

    def __repr__(self):
        return f'Token({self.type},{self.text},{repr(self.value)},{self.lineno},{self.begpos},{self.endpos})'


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
        'for':      'KW_FOR',
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
    t_OP_MINUS          = r'-'
    t_OP_MULT           = r'\*'
    t_OP_DIV            = r'\/'

    t_ignore_COMMENT    = r'\*\*\*.*$'
    t_ignore            = ' \t\v'

    # maximum identifier length is 31 characters long (like C)
    def t_ID(self, t):
        r'[_a-zA-Z]+'
        t.type = self.reserved.get(t.value, t.type)
        t.text = t.value
        if len(t.value) > 31:
            s = self.plextoken(t)
            print(f"{s.lineno}:{s.begpos},{s.endpos}: identifier too long: '{t.value}' truncated to '{t.value[:31]}'")
            t.value = t.value[:31]
        return t

    # maximum signed 32-bit is 0x7fffffff
    # value is truncated by taking 31 least significant bits
    def t_INT_LIT(self, t):
        r'\d+'
        INT32_MAX = 0x7fffffff
        t.text = t.value
        t.value = int(t.value)
        if t.value > INT32_MAX:
            v = t.value
            t.value &= INT32_MAX
            tok = self.plextoken(t)
            print(f"{tok.lineno}:{tok.begpos},{tok.endpos}: integer larger than 32-bits: {v} truncated to {t.value}")
        return t

    def t_newline(self, t):
        r'\r\n?|\n'
        self.lineno += 1

    def t_error(self, t):
        t = self.plextoken(t)
        print(f"{t.lineno}:{t.begpos},{t.begpos}: invalid char: '{t.value[0]}'")
        self.lexer.skip(1)

    def t_eof(self, t):
        more = self.src.readline()
        if more != '':
            self.lexer.input(more)
            return self.lexer.token()

    def __init__(self, src, parser=None, **kw):
        self.lexer = lex.lex(object=self, **kw)
        self.src = src
        self.lineno = 1
        self.tok = None
        self.parser = parser

    def plextoken(self, t):
        if t:
            t.lineno = self.lineno
            t.lexer = self
            return PlexToken.from_token(t)
        return t

    def ntok(self):
        return self.plextoken(self.lexer.token())

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


def main():
    f = sys.stdin if len(sys.argv) < 2 else open(sys.argv[1])
    scanner = Plexer(f)

    for t in scanner:
        print(t.type, end=' ')
        if t.type in ('ID', 'INT_LIT', 'COMMENT'):
            print(f'value {repr(t.value)}', end=' ')
        print(f'on line {t.lineno}, char {t.begpos} to {t.endpos}')

    f.close()

if __name__ == '__main__':
    main()
