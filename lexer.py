import toen
#Int,Float,String;
INTEGER = 'INTEGER'
FLOAT = 'FLOAT'
STRING = 'STRING'

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
SEMICOLON    = ';'
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

#Reserved Keywords: 
RESERVED_KEYWORDS = {
    'let': toen.Token(LET, 'let'),
    'var': toen.Token(VAR,'var'),
    'if': toen.Token(IF, 'if'),
    'else': toen.Token(ELSE, 'else'),
    'else if': toen.Token(ELSEIF,'else if'),
    'while': toen.Token(WHILE, 'while'),
}

class Lexer(object):
    def __init__(self, text):
        # text input is stored in a variable called self.text: "3 + 8"
        self.text = text
        # index/position where the lexer is currently looking at
        self.pos = 0
        # line of the text
        self.line = 1
        #stores the current token the lexer is looking at
        self.currentToken = None
        #stores the current character the lexer is looking at
        self.currentChar = self.text[self.pos]

    def error(self):
        raise Exception('Syntax Error')

    def advance(self):
        # advance the pos variable to go to the next char
        self.pos = self.pos + 1

        if self.pos >= len(self.text) :
            self.currentChar = None
        else:
            self.currentChar = self.text[self.pos]

    def peek(self, index):
        peekPos = self.pos + index
        if peekPos > len(self.text) - 1:
            return None
        else:
            return self.text[peekPos]

    def ignoreWhiteSpaces(self):
        # ignores white spaces
        while self.currentChar is not None and self.currentChar.isspace():
            if self.currentChar == '\n':
                self.line += 1
            self.advance()

    def number(self):
        # returns an integer/float from text
        number = ''
        while self.currentChar is not None and self.currentChar.isdigit():
            number = number + self.currentChar
            self.advance()

        if self.currentChar == '.':
            number = number + self.currentChar
            self.advance()

            while self.currentChar is not None and self.currentChar.isdigit():
                number = number + self.currentChar
                self.advance()

            num_tok = toen.Token('FLOAT', float(number))

        else:
            num_tok = toen.Token('INTEGER', int(number))

        return num_tok
    
    def string(self):
        string = ''
        while self.currentChar is not None and self.currentChar!='"':
            string += self.currentChar
            self.advance()
        return string
    
    def _id(self):
        # handles identifiers
        result = ''
        while self.currentChar is not None and self.currentChar.isalnum():
            result += self.currentChar
            self.advance()

        tok = RESERVED_KEYWORDS.get(result, toen.Token(ID, result))
        return tok

    def getNextToken(self):
        "Lexical Analyzer"
        "Breaks the input into tokens"

        while self.currentChar is not None:
            if self.currentChar.isspace():
                self.ignoreWhiteSpaces()
                continue
            if self.peek(0) =='l' and self.peek(1)=='e' and self.peek(2)=='t' and self.peek(3)==' ':
                for i in range(4):
                    self.advance()
                return toen.Token(LET,'let');
            if self.peek(0) =='v' and self.peek(1)=='a' and self.peek(2)=='r' and self.peek(3)==' ':
                for i in range(4):
                    self.advance()
                return toen.Token(VAR,'var');
            if self.peek(0) =='i' and self.peek(1)=='f' and self.peek(2)==' ':
                for i in range(3):
                    self.advance()
                return toen.Token(IF,'if');
            if self.peek(0) =='e' and self.peek(1)=='l' and self.peek(2)=='s' and self.peek(3)=='e' and self.peek(4)==' ':
                if self.peek(5)=='i' and self.peek(6)=='f' and self.peek(7)==' ':
                    for i in range(8):
                        self.advance()
                    return toen.Token(ELSEIF,'else if')
                for i in range(5):
                    self.advance()
                return toen.Token(ELSE,'else')
            if self.peek(0) =='w' and self.peek(1)=='h' and self.peek(2)=='i' and self.peek(3)=='l' and self.peek(4)=='e' and self.peek(5)==' ':
                for i in range(6):
                    self.advance()
                return toen.Token(WHILE,'while')
            if self.currentChar.isdigit():
                return self.number()
            if self.currentChar=='"':
                self.advance()
                return self.string()
            if self.currentChar.isalpha():
                return self._id()
            if self.currentChar == '+':
                self.advance()
                return toen.Token(PLUS, '+')

            if self.currentChar == '-':
                self.advance()
                return toen.Token(MINUS, '-')

            if self.currentChar == '*':
                self.advance()
                return toen.Token(MULTIPLY, '*')

            if self.currentChar == '/':
                self.advance()
                if self.currentChar == '/':
                    self.line+=1
                    return toen.TOKEN(COMMENT, '//')
                return toen.Token(DIVIDE, '/')

            if self.currentChar == '%':
                self.advance()
                return toen.Token(MODULO, '%')

            if self.currentChar == '(':
                self.advance()
                return toen.Token(LPAREN, '(')

            if self.currentChar == ')':
                self.advance()
                return toen.Token(RPAREN, ')')

            if self.currentChar == '{':
                self.advance()
                return toen.Token(LCURL, '{')

            if self.currentChar == '}':
                self.advance()
                return toen.Token(RCURL, '}')

            if self.currentChar == '=':
                self.advance()
                return toen.Token(EQUAL, '=')

            if self.currentChar == ';':
                self.advance()
                return toen.Token(SEMICOLON, ';')

            if self.currentChar == ',':
                self.advance()
                return toen.Token(COMMA, ',')

            self.error(
                message = "Invalid char {} at line {}".format(self.currentChar, self.line)
            )

        return toen.Token(EOF, None)
