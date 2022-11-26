###############################################################################
#                                                                             #
#  LEXER                                                                      #
#                                                                             #
###############################################################################

# Token types
#
# EOF (end-of-file) token is used to indicate that
# there is no more input left for lexical analysis
INTEGER       = 'INTEGER'
REAL          = 'REAL'
INTEGER_CONST = 'INTEGER_CONST'
REAL_CONST    = 'REAL_CONST'
PLUS          = 'PLUS'
MINUS         = 'MINUS'
MUL           = 'MUL'
FLOAT_DIV     = 'FLOAT_DIV'
MODULO        = 'MODULO'
LPAREN        = 'LPAREN'
RPAREN        = 'RPAREN'
ID            = 'ID'
ASSIGN        = 'ASSIGN'
BEGIN         = 'BEGIN'
END           = 'END'
SEMI          = 'SEMI'
DOT           = 'DOT'
PROGRAM       = 'PROGRAM'
VAR           = 'VAR'
COLON         = 'COLON'
COMMA         = 'COMMA'
PROCEDURE     = 'PROCEDURE'
EOF           = 'EOF'
#NEW
SYMBOL_SLASH  ='SYMBOL_SLASH'
LET           ='LET'
IF            ='IF'
ELSEIF        ='ELSEIF'
ELSE          ='ELSE'
WHILE         ='WHILE'
SWITCH        ='SWITCH'
CASE          ='CASE'
STRING        ='STRING'
STRING_       ='STRING_'
LCURL         ='LCURL'
RCURL         ='RCURL'
PRINT         ='PRINT'
EQUAL         ='=='
NOTEQUAL      ='!='
GREATERTHAN   ='>'
LESSERTHAN    ='<'
GRTR_OR_EQL   ='>='
LESR_OR_EQL   ='<='

class Token(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __str__(self):
        """String representation of the class instance.
        Examples:
            Token(INTEGER, 3)
            Token(PLUS, '+')
            Token(MUL, '*')
        """
        return 'Token({type}, {value})'.format(
            type=self.type,
            value=repr(self.value)
        )

    def __repr__(self):
        return self.__str__()


RESERVED_KEYWORDS = {
    'PROGRAM': Token(PROGRAM, 'PROGRAM'),
    'VAR': Token(VAR, 'var'),
    'DIV': Token(FLOAT_DIV, 'DIV'),
    'INTEGER': Token(INTEGER, 'INTEGER'),
    'REAL': Token(REAL, 'REAL'),
    'BEGIN': Token(BEGIN, 'BEGIN'),
    'END': Token(END, 'END'),
    'PROCEDURE': Token(PROCEDURE, 'PROCEDURE'),
    #NEW
    'LET': Token(LET,'let'),
    'IF': Token(IF,'if'),
    'ELSEIF': Token(ELSEIF, 'else if'),
    'ELSE': Token(ELSE,'else'),
    'WHILE': Token(WHILE,'while'),
    'SWITCH': Token(SWITCH,'SWITCH'),
    'CASE': Token(CASE,'CASE'),
    'STRING':Token(STRING,'STRING'),
    'PRINT':Token(PRINT,'print')
}

class Lexer(object):
    def __init__(self, text):
        # client string input, e.g. "4 + 2 * 3 - 6 / 2"
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char = self.text[self.pos]

    def error(self):
        print(self.current_char)
        print(self.get_next_token())
        raise Exception('Invalid character')

    def advance(self,n=1):
        """Advance the `pos` pointer and set the `current_char` variable."""
        self.pos += n
        if self.pos > len(self.text) - 1:
            self.current_char = None  # Indicates end of input
        else:
            self.current_char = self.text[self.pos]

    def peek(self, n=1):
        peek_pos = self.pos + n
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()
#SKIPS SINGLE LINE COMMENT USING //
    def skip_comment(self):
        while self.current_char != '\n':
            self.advance()
        #print("skip comment before", self.current_char)
        self.advance() 
        #print("skip comment after", self.current_char)
#SKIPS MULTI LINE COMMENT USING /*
    def skip_multi_line_comment(self):
        while  (self.current_char != '*' or self.peek()!='/') and self.current_char!=None:
            self.advance()
        if(self.current_char==None):
            self.error() 
        else:
            self.advance() # skipping *
        if(self.current_char=='/'):
            self.advance()
        else:
            self.error()

    def number(self):
        """Return a (multidigit) integer or float consumed from the input."""
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == '.':
            result += self.current_char
            self.advance()

            while (
                self.current_char is not None and
                self.current_char.isdigit()
            ):
                result += self.current_char
                self.advance()

            token = Token('REAL_CONST', float(result))
        else:
            token = Token('INTEGER_CONST', int(result))

        return token
    #NEW string function
    def string(self):
        string = ''
        while self.current_char is not None and self.current_char!='"':
            string += self.current_char
            self.advance()
        token =Token('STRING',str(string))
        self.advance()
        return token

    def _id(self):
        """Handle identifiers and reserved keywords"""
        result = ''
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char=='_'):
            result += self.current_char
            self.advance()

        token = RESERVED_KEYWORDS.get(result.upper(), Token(ID, result))
        return token

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)
        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:
            #print(self.current_char)
            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char == '/' and self.peek()=='*':
                self.advance()
                self.advance()
                self.skip_multi_line_comment()
                continue
            if self.current_char=='/' and self.peek()=='/':
                self.advance()
                self.advance()
                self.skip_comment()
                continue

            if self.current_char=='e' and self.peek()=='l' and self.peek(2)=='s' and self.peek(3)=='e' and self.peek(4)==' ' and self.peek(5)=='i' and self.peek(6)=='f':
                self.advance(7)
                return Token(ELSEIF, 'else if')
            if self.current_char.isalpha():
                return self._id()

            if self.current_char.isdigit():
                return self.number()

            if self.current_char=='{':
                self.advance()
                return Token(LCURL,'{')

            if self.current_char=='}':
                self.advance()
                return Token(RCURL,'}') 

            if self.current_char =='=':
                self.advance()
                if(self.current_char=='='):
                    self.advance()
                    return Token(EQUAL,'==')
                return Token(ASSIGN, '=')

            if self.current_char == ';':
                self.advance()
                return Token(SEMI, ';')

            if self.current_char == '%':
                self.advance()
                return Token(MODULO, '%')

            if self.current_char == ':':
                self.advance()
                return Token(COLON, ':')

            if self.current_char == ',':
                self.advance()
                return Token(COMMA, ',')

            

            if self.current_char == '+':
                self.advance()
                return Token(PLUS, '+')

            if self.current_char == '-':
                self.advance()
                return Token(MINUS, '-')

            if self.current_char == '*':
                self.advance()
                return Token(MUL, '*')

            if self.current_char == '/':
                self.advance()
                return Token(FLOAT_DIV, '/')

            if self.current_char == '(':
                self.advance()
                return Token(LPAREN, '(')

            if self.current_char=='"':
                self.advance()
                return self.string()
            if self.current_char == ')':
                self.advance()
                return Token(RPAREN, ')')

            if self.current_char == '.':
                self.advance()
                return Token(DOT, '.')

            if self.current_char == '!' and self.peek()=='=':
                self.advance(2)
                return Token(NOTEQUAL, '!=')

            if self.current_char == '>' and self.peek()=='=':
                self.advance(2)
                return Token(GRTR_OR_EQL, '>=')

            if self.current_char == '<' and self.peek()=='=':
                self.advance(2)
                return Token(LESR_OR_EQL, '<=')

            if self.current_char == '>':
                self.advance()
                return Token(GREATERTHAN, '>')

            if self.current_char == '<':
                self.advance()
                return Token(LESSERTHAN, '<')

            self.error()

        return Token(EOF, None)

class AST(object):
    pass

class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr

class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class String(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Compound(AST):
    """Represents a '{ ... }' block"""
    def __init__(self):
        self.children = []


class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class If(AST):
    def __init__(self, comparison, true_body, false_body):
        self.comparison = comparison
        self.true_body = true_body
        self.false_body = false_body

class While(AST):
    def __init__(self,comparison,body):
        self.comparison = comparison
        self.body = body

class var(AST):
    """The var node is constructed out of ID token."""
    def __init__(self, token):
        self.token = token
        self.value = token.value

class Print(AST):
    def __init__(self,token):
        self.type = token.type
        self.value = token.value

class PrintBinOp(AST):
    def __init__(self,BinOp):
        self.left = BinOp.left
        self.token = self.op = BinOp.op
        self.right = BinOp.right  

class PrintUnaryOp(AST):
    def __init__(self,UnaryOp):
        self.token = self.op = UnaryOp.op
        self.expr = UnaryOp.expr        

class NoOp(AST):
    pass


class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.lexer.get_next_token()
        self.line = 0;

    def error(self):
        print("Invalid syntax is occuring in line: ", self.line)
        print(self.current_token)
        raise Exception('Invalid syntax')

    def eat(self, token_type):
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token
        # and assign the next token to the self.current_token,
        # otherwise raise an exception.
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            print(token_type, " was not found")
            self.error()

    def program(self):
        """program : compound_statement DOT"""
        node = self.compound_statement()
        return node

    def compound_statement(self):
        """
        compound_statement: { statement_list }
        """
        nodes = self.statement_list()

        root = Compound()
        for node in nodes:
            #print(node)
            root.children.append(node)

        return root

    
    
    
    def block(self):
        """
        statement_list : statement
                       | statement SEMI statement_list
        """
        #print(self.current_token, " before eating lcurl")
        #self.lexer.skip_comment()
        self.eat(LCURL)
        #print(self.current_token, "after eating lcurl")
        '''if self.current_token in (print,let,var):
            self.eat(self.current_token.type)'''
        node = self.statement()

        results = [node]
        #self.eat(SEMI)
        #print(self.current_token)

        while self.current_token.type in (PRINT,LET,VAR,ID):
            
            #print(self.line)
            '''if self.current_token.type!=ID:
                print(self.current_token.type)
                self.eat(self.current_token.type)'''
            #print(self.current_token)
            results.append(self.statement())
            #print("statement added")
            #print(self.current_token)
            #self.eat(SEMI)
        #print("out of while loop")
        #print(self.lexer.current_char)
        #print(self.current_token)
        #self.lexer.skip_comment()
        '''if self.lexer.current_char==None :
            self.current_token = Token(EOF,None)
            return results'''
        #print(self.current_token)
        self.eat(RCURL)
        #print("rcurl removed")
        #self.lexer.skip_comment()
        return results

    def statement_list(self):
        """
        statement_list : statement
                       | statement SEMI statement_list
        """

        node = self.statement()
        #print(node)
        results = [node]

        while self.current_token.type != EOF:
            
            #print(self.line)
            #self.eat(SEMI)
            #print(self.statement())
            results.append(self.statement())

        return results

    def statement(self):
        
        """
        statement : compound_statement
                  | assignment_statement
                  | empty
        """
        self.line+=1
        #print("Statement ", self.line)
        #print(self.current_token)
        if self.current_token.type in (LET,VAR):
            self.eat(self.current_token.type)
            #print("declaration")
            node = self.assignment_statement()
            self.eat(SEMI)
        elif self.current_token.type==PRINT:
            self.eat(PRINT)
            #print("print")
            node = self.print_statement()
            self.eat(SEMI)
        elif self.current_token.type==ID:
            #print("assignment")
            node = self.assignment_statement()
            self.eat(SEMI)
        elif self.current_token.type==IF:
            self.eat(IF)
            #print("if")
            node = self.if_statement()
        elif self.current_token.type==WHILE:
            self.eat(WHILE)
            #print("while")
            node = self.while_statement()
        else:
            #print("end")
            #print(self.current_token.type)
            #self.current_token = Token(EOF,None);
            #self.lexer.skip_comment()
            #print(self.current_token, " ", self.lexer.get_next_token())
            node = self.empty()
        #print("Line ", self.line)
        return node

    def while_statement(self):
        self.eat(LPAREN)
        comparison = self.comparison()
        self.eat(RPAREN)
        body = self.block()
        #print("Out of block")
        node = While(comparison = comparison, body = body)
        #print(node.comparison, " ", node.body)
        return node

    def if_statement(self):
        self.eat(LPAREN)
        comparison = self.comparison()
        self.eat(RPAREN)
        
        #print("Lcurl eaten")
        #self.lexer.skip_comment()
        true_body = self.block()
        #print(self.current_token)
        false_body = self.empty()
        if self.current_token.type==ELSEIF:
            #print("Entered else if in if")
            false_body = self.build_elseif_tree()
            #leaf = elseif_comparison
        if self.current_token.type==ELSE:
            self.eat(ELSE)
            false_body = self.block()
        node = If(comparison, true_body, false_body)
        #print(node.comparison.left, " ", node.comparison.op, " ", node.comparison.right)
        #print(node.true_body)
        #print(node.false_body)
        return node

    def build_elseif_tree(self):
        self.eat(ELSEIF)
        self.eat(LPAREN)
        comparison = self.comparison()
        self.eat(RPAREN)
        true_body = self.block()
        false_body = self.empty()

        while self.current_token.type == ELSEIF:
            false_body = self.build_elseif_tree()
        if self.current_token.type == ELSE:
            #print("entered ELSE")
            self.eat(ELSE)
            false_body = self.block()
            #print("completed else body")
        node = If(comparison,true_body,false_body)
        #print(node.comparison.left, " ", node.comparison.op, " ", node.comparison.right)
        return node

    def print_statement(self):
        self.eat(LPAREN)
        if self.current_token.type==STRING:
            #print(self.current_token.value);
            node = Print(self.current_token)
            self.eat(STRING)
        elif self.current_token.type in (PLUS,MINUS):
            node = PrintUnaryOp(self.expr())
        else:
            varone = self.current_token
            self.eat(self.current_token.type)
            operator = self.current_token
            if operator.type == RPAREN:
                node = Print(varone)
            else:
                self.eat(operator.type)
                if varone.type==ID:
                    B = BinOp(left = var(varone), op = operator, right = self.expr())
                else:
                    B = BinOp(left = Num(varone), op = operator, right = self.expr())
                node = PrintBinOp(B)
                
            #print(self.expr().left, self.expr().op, self.expr().right)
            '''elif self.current_token.type==ID:
            node = self.empty()
            node = print(self.current_token)
            self.eat(ID)
        else:'''
        self.eat(RPAREN)
        return node

    def assignment_statement(self):
        """
        assignment_statement : variable ASSIGN expr
        """
        #print(self.current_token)
        left = self.variable()
        #print(left.value)
        token = self.current_token
        self.eat(ASSIGN)
        if(self.current_token.type==STRING):
            right = String(self.current_token)
            self.eat(STRING)
        else:
            right = self.expr()
        #print(right)
        node = Assign(left, token, right)
        return node

    def variable(self):
        """
        variable : ID
        """
        node = var(self.current_token)
        self.eat(ID)
        return node

    def empty(self):
        """An empty production"""
        return NoOp()

    def comparison(self):
        node = self.expr()
        if self.current_token.type in (EQUAL, NOTEQUAL, GREATERTHAN, LESSERTHAN, GRTR_OR_EQL,LESR_OR_EQL):
            token = self.current_token
            self.eat(token.type)
        else:
            print(self.current_token)
            raise Exception('Invalid comparison operators')
        node = BinOp(left=node, op=token, right = self.expr())
        #print(node.left, " ", node.op, " ", node.right)
        return node

    def factor(self):
        """factor : variable|Unary Op(Factor) |INTEGER| LPAREN expr RPAREN"""
        token = self.current_token
        if token.type == PLUS:
            self.eat(PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == MINUS:
            self.eat(MINUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type in (INTEGER_CONST,REAL_CONST):
            self.eat(token.type)
            return Num(token)
        elif token.type == LPAREN:
            self.eat(LPAREN)
            node = self.expr()
            self.eat(RPAREN)
            return node
        else:
            node = self.variable()
            return node

    def term(self):
        """term : factor ((MUL | DIV) factor)*"""
        node = self.factor()
        while self.current_token.type in (MUL, FLOAT_DIV, MODULO):
            token = self.current_token
            self.eat(token.type)
            node = BinOp(left=node, op=token, right=self.factor())

        return node

    def expr(self):
        """
        expr   : term ((PLUS | MINUS) term)*
        term   : factor ((MUL | DIV) factor)*
        factor : INTEGER | LPAREN expr RPAREN
        """
        node = self.term()

        while self.current_token.type in (PLUS, MINUS):
            token = self.current_token
            if token.type == PLUS:
                self.eat(PLUS)
            elif token.type == MINUS:
                self.eat(MINUS)

            node = BinOp(left=node, op=token, right=self.term())

        return node


    def parse(self):
        node = self.program()
        #print(node)
        if self.current_token.type != EOF:
            self.error()

        return node
###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################

class NodeVisitor(object):
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        print(node)
        raise Exception('No visit_{} method'.format(type(node).__name__))


class Interpreter(NodeVisitor):

    GLOBAL_SCOPE = {}

    def __init__(self, parser):
        self.parser = parser

    def visit_If(self, node):
        if self.visit(node.comparison)==True:
            #print("TRUE")
            self.visit(node.true_body)
        else:
            #print("FALSE")
            self.visit(node.false_body)

    def visit_While(self, node):
        while self.visit(node.comparison):
            #print("TRUE")
            self.visit(node.body)


    def visit_BinOp(self, node):
        #print(node.left, " ", node.right)
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == FLOAT_DIV:
            if node.left.token.type == REAL_CONST or node.right.token.type == REAL_CONST:
                return self.visit(node.left) / self.visit(node.right)
            return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == MODULO:
            return self.visit(node.left) % self.visit(node.right)
        elif node.op.type == EQUAL:
            return self.visit(node.left) == self.visit(node.right)
        elif node.op.type == NOTEQUAL:
            return self.visit(node.left) != self.visit(node.right)
        elif node.op.type == GREATERTHAN:
            return self.visit(node.left) > self.visit(node.right)
        elif node.op.type == LESSERTHAN:
            return self.visit(node.left) < self.visit(node.right)
        elif node.op.type == GRTR_OR_EQL:
            return self.visit(node.left) >= self.visit(node.right)
        elif node.op.type == LESR_OR_EQL:
            return self.visit(node.left) <= self.visit(node.right)

    def visit_Print(self,node):
        if node.type in (STRING, INTEGER_CONST):
            result = node.value
        else:
            result = self.GLOBAL_SCOPE.get(node.value)
        print(result)

    def visit_PrintBinOp(self,node):
        result = Interpreter.visit_BinOp(self,node)
        print(result)

    def visit_PrintUnaryOp(self,node):
        result = Interpreter.visit_UnaryOp(self,node)
        print(result)

    def visit_Num(self, node):
        #print(node.value)
        return node.value

    def visit_String(self, node):
        #print(node)
        return node.value

    def visit_UnaryOp(self, node):
        #print(node)
        op = node.op.type
        if op == PLUS:
            return +self.visit(node.expr)
        elif op == MINUS:
            return -self.visit(node.expr)

    def visit_list(self,node):
        for x in node:
            #print(x)
            self.visit(x)

    def visit_Compound(self, node):
        for child in node.children:
            #print(child)
            self.visit(child)

    def visit_Assign(self, node):
        #print(node.left, " ", node.right)
        var_name = node.left.value
        self.GLOBAL_SCOPE[var_name] = self.visit(node.right)

    def visit_var(self, node):
        #print(node)
        var_name = node.value
        val = self.GLOBAL_SCOPE.get(var_name)
        if val is None:
            raise NameError(repr(var_name))
        else:
            return val

    def visit_NoOp(self, node):
        pass

    def interpret(self):
        tree = self.parser.parse()
        if tree is None:
            return ''
        #print(tree)
        return self.visit(tree)


def main():
    file_name = 'testerlexer2.txt'
    with open(file_name) as f:
        text=f.read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    result = interpreter.interpret()
    #print(result)
    #print(Lexer.current_token)
    #print(parser.parse())
    print(interpreter.GLOBAL_SCOPE)


if __name__ == '__main__':
    main()
