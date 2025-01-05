##################################
############IMPORTS###############
##################################
import sys
from enum import Enum
from abc import ABC, abstractmethod
##################################
############CLASSES###############
##################################
class ParseError(RuntimeError):
    pass
##################################
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.current = 0

    def expression(self):
        return self.assignment()

    def assignment(self):
        expr = self._or()  # Parse the left-hand side of the assignment

        if self.match(TokenType.EQUAL):  # Check for '=' token
            equals = self.previous()  # Get the '=' token
            value = self.assignment()  # Recursively parse the right-hand side

            if isinstance(expr, Variable):  # Check if the left-hand side is a Variable expression
                name = expr.name  # Get the name from the Variable expression
                return Assign(name, value)  # Return the Assign expression

            # If the left-hand side is not a valid target for assignment, throw an error
            self.error(equals, "Invalid assignment target.")

        return expr  # If it's not an assignment, return the original expression

    def _or(self):
        expr = self._and()

        while self.match(TokenType.OR):
            operator = self.previous()
            right = self._and()
            expr = Logical(expr, operator, right)

        return expr

    def _and(self):
        expr = self.equality()

        while self.match(TokenType.AND):
            operator = self.previous()
            right = self.equality()
            expr = Logical(expr,operator,right)

        return expr

    def equality(self):
        expr = self.comparison()

        while self.match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL):
            operator = self.previous()
            right = self.comparison()
            expr = Binary(expr, operator, right)

        return expr

    def match(self, *types):
        for type in types:
            if self.check(type):
                self.advance()
                return True
        return False

    def check(self, type):
        if self.isAtEnd():
            return False
        return self.peek().type == type

    def advance(self):
        if not self.isAtEnd():
            self.current += 1
        return self.previous()

    def isAtEnd(self):
        return self.peek().type == TokenType.EOF

    def peek(self):
        return self.tokens[self.current]

    def previous(self):
        return self.tokens[self.current - 1]

    def comparison(self):
        expr = self.term()

        while self.match(TokenType.GREATER, TokenType.GREATER_EQUAL,
                         TokenType.LESS, TokenType.LESS_EQUAL):
            operator = self.previous()
            right = self.term()
            expr = Binary(expr, operator, right)
        return expr

    def term(self):
        expr = self.factor()

        while self.match(TokenType.MINUS, TokenType.PLUS):
            operator = self.previous()
            right = self.factor()
            expr = Binary(expr, operator, right)
        return expr

    def factor(self):
        expr = self.unary()

        while self.match(TokenType.SLASH, TokenType.STAR):
            operator = self.previous()
            right = self.unary()
            expr = Binary(expr, operator, right)

        return expr

    def unary(self):
        if self.match(TokenType.BANG, TokenType.MINUS):
            operator = self.previous()
            right = self.unary()
            return Unary(operator, right)
        return self.call()

    def call(self):
        expr = self.primary()

        while True:
            if self.match(TokenType.LEFT_PAREN):
                expr = self.finish_call(expr)
            else:
                break

        return expr

    def finish_call(self, callee):
        arguments = []

        if not self.check(TokenType.RIGHT_PAREN):
            while True:
                if arguments.__sizeof__() >= 255:
                    self.error(self.peek(), "Can't have more than 255 arguments.")

                arguments.append(self.expression())
                if not self.match(TokenType.COMMA):
                    break

        paren = self.consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.")

        return Call(callee, paren, arguments)

    def primary(self):
        # Handle literals
        if self.match(TokenType.FALSE):
            return Literal(False)
        if self.match(TokenType.TRUE):
            return Literal(True)
        if self.match(TokenType.NIL):
            return Literal(None)
        if self.match(TokenType.NUMBER, TokenType.STRING):
            return Literal(self.previous().literal)

        # Handle identifiers (variables)
        if self.match(TokenType.IDENTIFIER):  # This matches `a` in `print a;`
            return Variable(self.previous())  # Wraps the identifier in a Variable expression

        # Handle grouped expressions with parentheses
        if self.match(TokenType.LEFT_PAREN):
            expr = self.expression()
            self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
            return Grouping(expr)

        # If no valid primary expression is found, throw an error
        raise self.error(self.peek(), "Expect expression.")

    def consume(self, type, message):
        if self.check(type):
            return self.advance()

        raise ParseError(message)

    def error(self, token, message):
        Lox.error(token, message)
        return ParseError()

    def synchronize(self):
        self.advance()
        while not self.isAtEnd():
            if self.previous().type == TokenType.SEMICOLON:
                return

            if self.peek().type in {
                TokenType.CLASS, TokenType.FUN, TokenType.VAR, TokenType.IF,
                TokenType.WHILE, TokenType.PRINT, TokenType.RETURN,
            }:
                return

            self.advance()

    def parse(self):
        statements = []
        while not self.isAtEnd():
            stmt = self.declaration()
            if stmt is not None:  # Exclude None statements
                statements.append(stmt)
        return statements

    def declaration(self):
        try:
            if self.match(TokenType.FUN):
                return self.function("function")
            if self.match(TokenType.VAR):
                return self.varDeclaration()

            return self.statement()

        except ParseError:
            self.synchronize()
            return None

    def function(self, kind):
        name = self.consume(TokenType.IDENTIFIER, "Expect " + kind + " name.")
        self.consume(TokenType.LEFT_PAREN, "Expect '(' after " + kind + " name.")
        parameters = []
        if not self.check(TokenType.RIGHT_PAREN):
            while True:
                if parameters.__sizeof__() >= 255:
                    self.error(self.peek(), "Can't have more than 255 parameters.")

                parameters.append(
                    self.consume(TokenType.IDENTIFIER, "Expect parameter name.")
                )

                if not self.match(TokenType.COMMA):
                    break
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters")

        self.consume(TokenType.LEFT_BRACE, "Expect '{' before " + kind + " body.")
        body = self.block()
        return Function(name, parameters, body)

    def statement(self):
        if self.match(TokenType.FOR):
            return self.for_statement()
        if self.match(TokenType.IF):
            return self.if_Statement()
        if self.match(TokenType.PRINT):
            return self.printStatement()
        if self.match(TokenType.RETURN):
            return self.return_statement()
        if self.match(TokenType.WHILE):
            return self.while_statement()
        if self.match(TokenType.LEFT_BRACE):
            return Block(self.block())

        return self.expressionStatement()

    def return_statement(self):
        keyword = self.previous()
        value = None
        if not self.check(TokenType.SEMICOLON):
            value = self.expression()

        self.consume(TokenType.SEMICOLON, "Expect ':' after return value.")
        return Return(keyword, value)

    def for_statement(self):
        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.")

        initializer = None
        if self.match(TokenType.SEMICOLON):
            initializer = None
        elif self.match(TokenType.VAR):
            initializer = self.varDeclaration()
        else:
            initializer = self.expressionStatement()

        condition = None
        if not self.check(TokenType.SEMICOLON):
            condition = self.expression()

        self.consume(TokenType.SEMICOLON, "Expect ';' after loop condition.")

        increment = None
        if not self.check(TokenType.RIGHT_PAREN):
            increment = self.expression()
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.")
        body = self.statement()

        if increment is not None:
            body = Block([body, Expression(increment)])

        if condition is None:
            condition = Literal(True)
        body = While(condition, body)

        if initializer is not None:
            body = Block([initializer, body])

        return body

    def while_statement(self):
        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.")
        condition = self.expression()
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition")
        body = self.statement()

        return While(condition, body)

    def if_Statement(self):
        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
        condition = self.expression()
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition")

        then_branch = self.statement()
        else_branch = None

        if self.match(TokenType.ELSE):
            else_branch = self.statement()

        return If(condition, then_branch, else_branch)

    def block(self):
        statements = []

        while not self.check(TokenType.RIGHT_BRACE) and not self.isAtEnd():
            statements.append(self.declaration())

        self.consume(TokenType.RIGHT_BRACE, "Expect '}' after block.")
        return statements

    def printStatement(self):
        value = self.expression()
        self.consume(TokenType.SEMICOLON, "Expect ';' after value.")
        return Print(value)

    def varDeclaration(self):
        name = self.consume(TokenType.IDENTIFIER, "Expect variable name.")

        initializer = None
        if self.match(TokenType.EQUAL):
            initializer = self.expression()

        self.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")
        return Var(name, initializer)

    def expressionStatement(self):
        expr = self.expression()
        self.consume(TokenType.SEMICOLON, "Expect ';' after expression.")
        return Expression(expr)
##################################
class Visitor(ABC):
    @abstractmethod
    def visit_expression_stmt(self, stmt):
        pass

    def visit_print_stmt(self, stmt):
        pass

    def visit_variable(self, expr):
        pass

    def visit_assign_expr(self, expr):
        pass

    def print (self, expr):
        return expr.accept(self)
    @abstractmethod
    def visit_binary(self, expr):
        return self.parenthesize(expr.operator.lexeme, expr.left, expr.right)

    @abstractmethod
    def visit_grouping(self, expr):
        return self.parenthesize("group", expr.expression)

    @abstractmethod
    def visit_literal(self, expr):
        if expr.value == None:
            return "nil"
        return expr.value

    @abstractmethod
    def visit_unary(self, expr):
        return self.parenthesize(expr.operator.lexeme, expr.right)

    def parenthesize(self, name, *args):
        stringBuilder = StringBuilder()

        stringBuilder.append('(').append(name)
        for arg in args:
            stringBuilder.append(' ')
            stringBuilder.append(arg.accept(self))

        stringBuilder.append(')')
        return stringBuilder.to_string()
##################################
class Stmt(ABC):
    @abstractmethod
    def accept(self, visitor):
        pass
##################################
class If(Stmt):
    def __init__(self, condition, then_branch, else_branch):
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch

    def accept(self, visitor):
        return visitor.visit_if_stmt(self)
##################################
class Var(Stmt):
        def __init__(self, name, initializer):
            self.name = name
            self.initializer = initializer

        def accept(self, visitor):
            return visitor.visit_var_stmt(self)
##################################
class While(Stmt):
    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

    def accept(self, visitor):
        return visitor.visit_while_stmt(self)
##################################
class Block(Stmt):
    def __init__(self, statements):
        self.statements = statements

    def accept(self, visitor):
        return visitor.visit_block_statement(self)
##################################
class Return(Stmt):
    def __init__(self, keyword, value):
        self.keyword = keyword
        self.value = value

    def accept(self, visitor):
        return visitor.visit_return_stmt(self)
##################################
class Expression(Stmt):
    def __init__(self, expression):
        self.expression = expression

    def accept(self, visitor):
        return visitor.visit_expression_stmt(self)
##################################
class Print (Stmt):
    def __init__(self, expression):
        self.expression = expression

    def accept(self, visitor):
        return visitor.visit_print_stmt(self)
##################################
class StringBuilder:
    def __init__(self):
        self.parts = []

    def append(self, text):
        self.parts.append(text)
        return self
    def to_string(self):
        return ''.join(self.parts)
##################################
class Expr(ABC):
    @abstractmethod
    def accept(self, visitor):
        pass
##################################
class Call(Expr):
    def __init__(self, callee, paren, arguments):
        self.callee = callee
        self.paren = paren
        self.arguments = arguments

    def accept(self, visitor):
        return visitor.visit_call_expr(self)
##################################
class Logical(Expr):
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

    def accept(self, visitor):
        return visitor.visit_logical_expr(self)
#################################
class Binary(Expr):
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

    def accept(self, visitor):
        return visitor.visit_binary(self)
##################################
class Grouping(Expr):
    def __init__(self, expression):
        self.expression = expression

    def accept(self, visitor):
        return visitor.visit_grouping(self)
##################################
class Literal(Expr):
    def __init__(self, value):
        self.value = value

    def accept(self, visitor):
        return visitor.visit_literal(self)
##################################
class Unary(Expr):
    def __init__(self, operator, right):
        self.operator = operator
        self.right = right

    def accept(self, visitor):
        return visitor.visit_unary(self)
##################################
class Variable():
    def __init__(self, *args):
        self.name = args[0]
        if len(args) == 2:
            self.intializer = args[1]

    def accept(self, visitor):
        return visitor.visit_variable(self)
##################################
class TokenType(Enum):
    LEFT_PAREN = "("
    RIGHT_PAREN = ")"
    LEFT_BRACE = "{"
    RIGHT_BRACE = "}"
    COMMA = ","
    DOT = "."
    MINUS = "-"
    PLUS = "+"
    SEMICOLON = ";"
    SLASH = "/"
    STAR = "*"
    BANG = "!"
    BANG_EQUAL = "!="
    EQUAL = "="
    EQUAL_EQUAL = "=="
    GREATER = ">"
    GREATER_EQUAL = ">="
    LESS = "<"
    LESS_EQUAL = "<="
    IDENTIFIER = "identifier"
    STRING = "string"
    NUMBER = "number"
    AND = "and"
    CLASS = "class"
    ELSE = "else"
    FALSE = "false"
    FUN = "fun"
    FOR = "for"
    IF = "if"
    NIL = "nil"
    OR = "or"
    PRINT = "print"
    RETURN = "return"
    SUPER = "super"
    THIS = "this"
    VAR = "var"
    WHILE = "while"
    EOF = "eof"
    TRUE = "true"
    RUN = "run"
##################################
class Token:
    def __init__(self, type: TokenType, lexeme: str, literal, line: int):
        self.type = type
        self.lexeme = lexeme
        self.literal = literal
        self.line = line

    def __str__(self):
        return f"{self.type} {self.lexeme} {self.literal}"
##################################
class Scanner:
    def __init__(self, source: str):
        self.source = source
        self.tokens = []
        self.start = 0
        self.current = 0
        self.line = 1

    reserved_words = {
        "and": TokenType.AND,
        "class": TokenType.CLASS,
        "else": TokenType.ELSE,
        "false": TokenType.FALSE,
        "fun": TokenType.FUN,
        "for": TokenType.FOR,
        "if": TokenType.IF,
        "nil": TokenType.NIL,
        "or": TokenType.OR,
        "print": TokenType.PRINT,
        "return": TokenType.RETURN,
        "super": TokenType.SUPER,
        "this": TokenType.THIS,
        "var": TokenType.VAR,
        "while": TokenType.WHILE,
        "true": TokenType.TRUE,
    }

    def scan_tokens(self):
        while not self.is_at_end():
            self.start = self.current
            self.scan_token()
        self.tokens.append(Token(TokenType.EOF, "", None, self.line))
        return self.tokens

    def scan_token(self):
        c = self.advance()
        #Tokens with exactly one character
        if c == '(':
            self.add_token(TokenType.LEFT_PAREN)
        elif c == ')':
            self.add_token(TokenType.RIGHT_PAREN)
        elif c == '{':
            self.add_token(TokenType.LEFT_BRACE)
        elif c == '}':
            self.add_token(TokenType.RIGHT_BRACE)
        elif c == ',':
            self.add_token(TokenType.COMMA)
        elif c == '.':
            self.add_token(TokenType.DOT)
        elif c == '-':
            self.add_token(TokenType.MINUS)
        elif c == '+':
            self.add_token(TokenType.PLUS)
        elif c == ';':
            self.add_token(TokenType.SEMICOLON)
        elif c == '*':
            self.add_token(TokenType.STAR)
        #Tokens made of one or two characters
        elif c == '!':
            self.add_token(TokenType.BANG_EQUAL if self.match('=') else TokenType.BANG)
        elif c == '=':
            self.add_token(TokenType.EQUAL_EQUAL if self.match('=') else TokenType.EQUAL)
        elif c == '<':
            self.add_token(TokenType.LESS_EQUAL if self.match('=') else TokenType.LESS)
        elif c == '>':
            self.add_token(TokenType.GREATER_EQUAL if self.match('=') else TokenType.GREATER)
        #Token that may or may not be a comment
        elif c == '/':
            if self.match('/'):
                while self.peek() != '\n' and not self.is_at_end():
                    self.advance()
            else:
                self.add_token(TokenType.SLASH)
        #Tokens that are whitespace
        elif c in {' ', '\r', '\t'}:
            pass
        elif c == '\n':
            self.line += 1
        elif c == '"':
            self.string()
        elif self.is_digit(c):
            self.number()
        elif c == 'o':
            if self.match('r'):
                self.add_token(TokenType.OR)
        else:
            if self.is_digit(c):
                self.number()
            elif self.isAlpha(c):
                self.identifier()
            else:
                Lox.error(self.line, f"Unexpected character: {c}")

    def string(self):
        while self.peek() != '"' and not self.is_at_end():
            if self.peek() == '\n':
                self.line += 1
            self.advance()

        if self.is_at_end():
            Lox.error(self.line, "Unterminated string.")
            return

        self.advance()  # Closing quote
        text = self.source[self.start + 1:self.current - 1]
        self.add_token(TokenType.STRING, text)

    def number(self):
        while self.is_digit(self.peek()):
            self.advance()

        if self.peek() == '.' and self.is_digit(self.peek_next()):
            self.advance()
            while self.is_digit(self.peek()):
                self.advance()

        value = float(self.source[self.start:self.current])
        self.add_token(TokenType.NUMBER, value)

    def match(self, expected: str):
        if self.is_at_end() or self.source[self.current] != expected:
            return False
        self.current += 1
        return True

    def peek(self):
        return '\0' if self.is_at_end() else self.source[self.current]

    def peek_next(self):
        return '\0' if self.current + 1 >= len(self.source) else self.source[self.current + 1]

    def is_digit(self, c: str):
        return '0' <= c <= '9'

    def is_at_end(self):
        return self.current >= len(self.source)

    def advance(self):
        self.current += 1
        return self.source[self.current - 1]

    def add_token(self, type: TokenType, literal=None):
        text = self.source[self.start:self.current]
        self.tokens.append(Token(type, text, literal, self.line))

    def identifier(self):
        while self.isAlphaNumeric(self.peek()):
            self.advance()

        # Resolve reserved keywords (e.g., `print`, `class`) or fallback to IDENTIFIER
        text = self.source[self.start:self.current]
        type = self.reserved_words.get(text, TokenType.IDENTIFIER)  # Default to IDENTIFIER
        self.add_token(type)

    def isAlpha(self,c):
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_'
    def isAlphaNumeric(self,c):
        return self.isAlpha(c) or self.is_digit(c)
##################################
class Runtime_Error(Exception):
    def __init__(self, token, message):
        self.token = token
        self.message = message
        Exception.__init__(self, message)
##################################
class Assign(Expr):
    def __init__(self, name: Token, value: Expr):
        self.name = name # The variable being assigned to
        self.value = value # The new value being assigned

    def accept(self, visitor):
        return visitor.visit_assign_expr(self) # Call the corresponding visitor method
##################################
class Environment:
    def __init__(self, enclosing=None):
        self.enclosing = enclosing
        self.values = {}

    def define(self, name, value):
        self.values[name] = value #Add the variable to the current environment

    def get(self, name: Token):
        lexeme = name.lexeme  # The variable name as a string
        if lexeme in self.values:  # Check if the variable exists in the current scope
            return self.values[lexeme]

        if self.enclosing is not None:  # Check in outer scope
            return self.enclosing.get(name)

        # Throw an error if the variable isn't found
        raise Runtime_Error(name, f"Undefined variable '{lexeme}'.")

    def assign(self, name: Token, value):
        # Check if the variable exists in the current environment
        if name.lexeme in self.values:
            # Update the variable's value
            self.values[name.lexeme] = value
            return

        # If the variable is not defined, throw a runtime error
        if self.enclosing is not None:
            # Check in the enclosing environment (if in a nested scope)
            self.enclosing.assign(name, value)
            return

        # Throw an error if the variable isn't found in any scope
        raise Runtime_Error(name, f"Undefined variable '{name.lexeme}'.")
##################################
class Lox_Callable(ABC):
    @abstractmethod
    def arity(self):
        pass
    @abstractmethod
    def call (self, interpreter, arguments):
        pass
##################################
class Clock(Lox_Callable):
    def arity(self):
        return 0

    def call(self, interpreter, arguments):
        import time
        return time.time()

    def __str__(self):
        return "<native fn>"
##################################
class Function(Stmt):
    def __init__(self, name, params, body):
        self.name = name
        self.params = params
        self.body = body

    def accept(self, visitor):
        return visitor.function_stmt(self)
##################################
class Return_Exception(RuntimeError):
    def __init__(self,value):
        RuntimeError(None,None, False, False)
        self.value = value
##################################
class Interpreter(Visitor):
    def __init__(self):
        self.globals = Environment()
        self.environment = self.globals

        self.globals.define("clock", Clock())

    def visit_literal(self, literal: Literal):
        return literal.value

    def visit_unary(self, expr:Unary):
        right = self.evaluate(expr.right)

        if expr.operator.type == TokenType.BANG:
            return not self.isTruthy(right)
        elif expr.operator.type == TokenType.MINUS:
            self.checkNumberOperand(expr.operator, right)
            return -right


        return None

    def visit_return_stmt(self, stmt: Return):
        value = None
        if stmt.value is not None:
            value = self.evaluate(stmt.value)

        raise Return_Exception(value)

    def visit_variable(self, expr: Variable):
        return self.environment.get(expr.name)  # Retrieve the variable's value from the environment

    def checkNumberOperand(self, operator, operand):
        if isinstance(operand, (float, int)):
            return
        raise Runtime_Error(operator, "Operand must be a number.")

    def isTruthy(self, object):
        if object is None:
            return False
        if isinstance(object, bool):
            return bool(object)

        return True

    def visit_grouping(self, expr: Grouping):
        return self.evaluate(expr.expression)

    def evaluate(self, expr:Expr):
        return expr.accept(self)

    def visit_expression_stmt(self, stmt: Expression):
        self.evaluate(stmt.expression)
        return None

    def visit_print_stmt(self, stmt):
        value = self.evaluate(stmt.expression)
        print(self.stringify(value))
        return None

    def visit_var_stmt(self, stmt: Var):
        value = None
        if stmt.initializer is not None:
            value = self.evaluate(stmt.initializer)  # Evaluate the initializer

        self.environment.define(stmt.name.lexeme, value)  # Define the variable
        return None  # Variable declarations don't produce a value

    def visit_assign_expr(self, expr: Assign):
        # Step 1: Evaluate the right-hand side of the assignment (the value being assigned).
        value = self.evaluate(expr.value)

        # Step 2: Assign this value to the variable in the environment.
        # `expr.name` represents the token of the variable being assigned to.
        self.environment.assign(expr.name, value)

        # Step 3: Return the value assigned (useful if the assignment is part of a larger expression).
        return value

    def visit_binary(self, expr: Binary):
        left = self.evaluate(expr.left)
        right = self.evaluate(expr.right)

        if expr.operator.type == TokenType.GREATER:
            self.checkNumberOperands(expr.operator, left, right)
            return left > right
        elif expr.operator.type == TokenType.GREATER_EQUAL:
            self.checkNumberOperands(expr.operator, left, right)
            return left >= right
        elif expr.operator.type == TokenType.LESS:
            self.checkNumberOperands(expr.operator, left, right)
            return left < right
        elif expr.operator.type == TokenType.LESS_EQUAL:
            self.checkNumberOperands(expr.operator, left, right)
            return left <= right
        elif expr.operator.type == TokenType.MINUS:
            self.checkNumberOperands(expr.operator, left, right)
            return left - right
        elif expr.operator.type == TokenType.PLUS:
            if isinstance(left, (float, int)) and isinstance(right, (float, int)):
                return left + right

            if isinstance(left, str) and isinstance(right, str):
                return left + right

            raise Runtime_Error(expr.operator, "Operand must be a number.")
        elif expr.operator.type == TokenType.SLASH:
            self.checkNumberOperands(expr.operator, left, right)
            if right == 0:
                raise Runtime_Error(expr.operator, "Division by zero")
            return left / right
        elif expr.operator.type == TokenType.STAR:
            self.checkNumberOperands(expr.operator, left, right)
            return left * right
        elif expr.operator.type == TokenType.BANG_EQUAL:
            return not self.isEqual(left, right)
        elif expr.operator.type == TokenType.EQUAL_EQUAL:
            return self.isEqual(left, right)
        return None

    def visit_if_stmt(self, stmt:If):
        if self.isTruthy(self.evaluate(stmt.condition)):
            self.execute(stmt.then_branch)
        elif stmt.else_branch is not None:
            self.execute(stmt.else_branch)

        return None

    def visit_while_stmt(self, stmt: While):
        while self.isTruthy(self.evaluate(stmt.condition)):
            self.execute(stmt.body)

        return None

    def visit_logical_expr(self, expr :  Logical):
        left = self.evaluate(expr.left)

        if  expr.operator.type == TokenType.OR:
            if self.isTruthy(left):
                return left
        else:
            if not self.isTruthy(left):
                return left

        return self.evaluate(expr.right)

    def isEqual(self, left, right):
        if left is None and right is None:
            return True
        if left is None:
            return False

        return left is right

    def checkNumberOperands(self, operator:Token, left, right):
        if isinstance(left, (float,int)) and isinstance(right, (float,int)):
            return
        raise Runtime_Error(operator, "Operand must be a number.")

    def interpret(self, statements):
        statements = [stmt for stmt in statements if stmt is not None]  # Exclude any None
        try:
            for statement in statements:
                self.execute(statement)
        except Runtime_Error as e:
            Lox.runtimeError(e)

    def execute(self, statement):
        statement.accept(self) #TODO

    def visit_block_statement(self, stmt):
        self.execute_block(stmt.statements, Environment(self.environment))
        return None

    def visit_call_expr(self, expr: Call):
        callee = self.evaluate(expr.callee)

        arguments = []

        for argument in expr.arguments:
            arguments.append(self.evaluate(argument))

        if not isinstance(callee, Lox_Callable):
            raise Runtime_Error(expr.paren, "Can only call functions and classes.")

        # function = Lox_Callable()
        # return function.call(callee, arguments)
        return callee.call(self, arguments)

    def function_stmt(self, stmt: Function):
        function = Lox_Function(self.environment, stmt)
        self.environment.define(stmt.name.lexeme, function)
        return None

    def execute_block(self, statements, environment):
        previous = self.environment
        try:
            self.environment = environment

            for statement in statements:
                self.execute(statement)
        finally:
            self.environment = previous

    def stringify(self, object):
        if object is None:
            return "nil"

        if isinstance(object, float):
            text = object.__str__()
            if text.endswith(".0"):
                text = text[0: len(text) - 2]
            return text

        return str(object)
##################################
class Lox_Function(Lox_Callable):
    def __init__(self,closure, declaration=None):
        self.declaration = declaration
        self.closure = closure

    def call(self, interpreter:Interpreter, arguments):
        environment = Environment(self.closure)
        for i in range(len(self.declaration.params)):
            environment.define(self.declaration.params[i].lexeme, arguments[i])

        try:
            interpreter.execute_block(self.declaration.body, environment)
        except Return_Exception as returnValue:
            return returnValue.value

        return None

    def arity(self):
        return len(self.declaration.params)

    def __str__(self):
        return "<fn " + self.declaration.name.lexeme + ">"
##################################
class Lox:
    interpreter = Interpreter()
    had_error = False
    had_runtimeError = False

    @staticmethod
    def main(args):
        if len(args) > 1:
            print("Usage: python lox.py [script]")
            sys.exit(64)
        elif len(args) == 1:
            Lox.run_file(args[0])
        else:
            Lox.run_prompt()

    @staticmethod
    def run_file(path: str):
        with open(path, 'r', encoding='utf-8') as file:
            Lox.run(file.read())
        if Lox.had_error:
            sys.exit(65)
        if Lox.had_runtimeError:
            sys.exit(70)

    @staticmethod
    def run_prompt():
        while True:
            try:
                line = input("> ")
                if line is None:
                    break
                Lox.run(line)
                Lox.had_error = False
            except EOFError:
                break

    @staticmethod
    def run(source: str):
        scanner = Scanner(source)
        tokens = scanner.scan_tokens()


        parser = Parser(tokens)
        statements = parser.parse()


        if Lox.had_error:
            return

        Lox.interpreter.interpret(statements)

    @staticmethod
    def report(line: int, where: str, message: str):
        print(f"[line {line}] Error{where}: {message}", file=sys.stderr)
        Lox.had_error = True

    @staticmethod
    def error(token: Token, message):
        if token.type == TokenType.EOF:
            Lox.report(token.line, " at end", message)
        else:
            Lox.report(token.line, f" at '{token.lexeme}'", message)
    @staticmethod
    def runtimeError(error:Runtime_Error):
        print(f"{error.message}\n[line {error.token.line}]", file=sys.stderr)
        Lox.had_runtimeError = True
##################################
class AstPrinter(Visitor):
    def visit_binary(self, expr):
        return self.parenthesize(expr.operator.lexeme, expr.left, expr.right)

    def visit_grouping(self, expr):
        return self.parenthesize("group", expr.expression)

    def visit_literal(self, expr):
        if expr.value is None:
            return "nil"
        return str(expr.value)

    def visit_unary(self, expr):
        return self.parenthesize(expr.operator.lexeme, expr.right)


if __name__ == "__main__":
     Lox.main(sys.argv[1:])