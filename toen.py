INTEGER = 'INTEGER'
FLOAT = 'FLOAT'
STRING ='STRING'

PLUS = 'PLUS'
MINUS = 'MINUS'
MULTIPLY = 'MULTIPLY'
DIVIDE = 'DIVIDE'
MODULO = 'MODULO'

LPAREN = '('
RPAREN = ')'
LCURL = '{'
RCURL = '}'

EQUAL = '='
SEMICOLON = ';'
ID = 'ID'
COMMA = ','
DOT = '.'

COMMENT = '//'

WHILE = 'while'
IF = 'if'
ELSE = 'else'
ELSEIF = 'else if'
LET = 'let'
VAR = 'var'

EOF = 'EOF'

class Token(object):
    def __init__(self, type, value):
        # stores the token type: INTEGER, PLUS, MINUS, MULTIPLY, DIVIDE, LPAREN, RPAREN, MODULO, EOF
        self.type = type
        # stores the value: +, -, *, /, %, None
        self.value = value

    def __str__(self):
        """String representation of the class instance.
        Examples:
            Token(INTEGER, 6)
            Token(MINUS, '-')
        """
        return 'Token({type}, {value})'.format(type = self.type, value = repr(self.value))

    def __repr__(self):
        return self.__str__()
