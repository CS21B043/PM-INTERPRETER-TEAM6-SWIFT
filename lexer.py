
import token
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

FOR = 'FOR'
IF = 'IF'
ELSE = 'ELSE'
LET = 'LET'
VAR = 'VAR'

EOF = 'EOF'

#Reserved Keywords: 
RESERVED_KEYWORDS = {
    'let': token.Token(LET, 'let'),
    'var': token.Token(VAR,'var'),
    'if': token.Token(IF, 'if'),
    'else': token.Token(ELSE, 'else'),
    'for': token.Token(FOR, 'for'),
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

            num_tok = token.Token('FLOAT', float(number))

        else:
            num_tok = token.Token('INTEGER', int(number))

        return num_tok

    def _id(self):
        # handles identifiers and reserved keywords
        result = ''
        while self.currentChar is not None and self.currentChar.isalnum():
            result += self.currentChar
            self.advance()

        tok = RESERVED_KEYWORDS.get(result, token.Token(ID, result))
        return tok

    def getNextToken(self):
        "Lexical Analyzer"
        "Breaks the input into tokens"

        while self.currentChar is not None:
            if self.currentChar.isspace():
                self.ignoreWhiteSpace()
                continue

            if self.currentChar.isdigit():
                return self.number()

            if self.currentChar.isalpha():
                return self._id()

            if self.currentChar == '+':
                self.advance()
                return token.Token(PLUS, '+')

            if self.currentChar == '-':
                self.advance()
                return token.Token(MINUS, '-')

            if self.currentChar == '*':
                self.advance()
                return token.Token(MULTIPLY, '*')

            if self.currentChar == '/':
                self.advance()
                return token.Token(DIVIDE, '/')

            if self.currentChar == '%':
                self.advance()
                return token.Token(MODULO, '%')

            if self.currentChar == '(':
                self.advance()
                return token.Token(LPAREN, '(')

            if self.currentChar == ')':
                self.advance()
                return token.Token(RPAREN, ')')

            if self.currentChar == '{':
                self.advance()
                return token.Token(LCURL, '{')

            if self.currentChar == '}':
                self.advance()
                return token.Token(RCURL, '}')

            if self.currentChar == '=':
                self.advance()
                return token.Token(EQUAL, '=')

            if self.currentChar == ';':
                self.advance()
                return token.Token(SEMICOLON, ';')

            if self.currentChar == ',':
                self.advance()
                return token.Token(COMMA, ',')

            self.error(
                message = "Invalid char {} at line {}".format(self.currentChar, self.line)
            )

        return token.Token(EOF, None)