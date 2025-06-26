package parser

import (
	"fmt"
	"interpreter/ast"
	"interpreter/lexer"
	"interpreter/token"
	"strconv"
)

const (
	_ int = iota
	LOWEST
	ASSIGNMENT
	CONDITIONAL
	SUM
	PRODUCT
	EXPONENT
	PREFIX
	POSTFIX
	CALL
	// LOWEST
	// EQUALS
	// LESSGREATER
)

type Parser struct {
	l *lexer.Lexer

	errors    []string
	curToken  token.Token
	peekToken token.Token

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
	Precedences    map[token.TokenType]int
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l, errors: []string{}}

	// prefix
	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)

	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)

	p.registerPrefixOperators(token.PLUS)
	p.registerPrefixOperators(token.MINUS)
	p.registerPrefixOperators(token.BANG)
	p.registerPrefixOperators(token.ASTERISK)

	p.infixParseFns = make(map[token.TokenType]infixParseFn)

	// infix

	p.registerInfixOperators(token.PLUS)
	p.registerInfixOperators(token.ASSIGN)
	p.registerInfixOperators(token.ASTERISK)
	p.registerInfixOperators(token.SLASH)
	p.registerInfixOperators(token.MINUS)
	p.registerInfixOperators(token.BANG)

	// precedence

	p.Precedences = map[token.TokenType]int{
		token.PLUS:     SUM,
		token.MINUS:    SUM,
		token.ASTERISK: PRODUCT,
	}

	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseInfixOperator(left ast.Expression) ast.Expression {

	infix := &ast.InfixExpression{Left: left}

	operator := p.curToken

	infix.Token = operator

	p.nextToken()

	right := p.parseExpression(p.getPrecedence(operator.Type))

	infix.Right = right

	return infix
}
func (p *Parser) parsePrefixOperator() ast.Expression {
	operator := p.curToken

	p.nextToken()

	operand := p.parseExpression(p.getPrecedence(operator.Type))
	return &ast.PrefixExpression{Token: operator, Operand: operand}
}

func (p *Parser) registerPrefixOperators(tt token.TokenType) {
	p.registerPrefix(tt, p.parsePrefixOperator)
}

func (p *Parser) registerInfixOperators(tt token.TokenType) {
	p.registerInfix(tt, p.parseInfixOperator)
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead.", t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.curToken}

	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)

	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	lit.Value = value
	return lit
}
func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement()

		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}

		p.nextToken()
	}
	return program
}

// note: 		{token.LET, "let"},
// {token.IDENT, "five"},
// {token.ASSIGN, "="},
// {token.INT, "5"},
// {token.SEMICOLON, ";"},

// Statement produces no value
// An identifier is a expression
// let a is a statement, if curToken != token.LET { return nil }
// parseStatement needs to add Program Root Node (is a statement)

// to parse 'let a = 0', Token.LET, Token.IDENT, Token.ASSIGN, token.INT

func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

// parse statements
func (p *Parser) parseExpression(precedence int) ast.Expression {

	prefix := p.prefixParseFns[p.curToken.Type]

	if prefix == nil {
		return nil
	}

	leftExp := prefix()

	fmt.Println("Parsing Prefix Expression: ", leftExp)

	// precedence example:
	// a + b * c;

	// prefix identifier a, infix, left = a, operator = +, now at b,
	fmt.Printf("Precedence of left: %d, Precedence of right: %d\n", precedence, p.getCurrentPrecedence())

	for !p.curTokenIs(token.SEMICOLON) || precedence < p.getCurrentPrecedence() {

		peekExp := p.peekToken

		infix := p.infixParseFns[peekExp.Type]

		if infix == nil {
			return leftExp
		}

		p.nextToken()

		leftExp = infix(leftExp)

		fmt.Println("Parsing Infix Expression: ", leftExp)
	}

	return leftExp
}
func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.curToken}

	stmt.Expression = p.parseExpression(LOWEST)

	fmt.Printf("Parsing Expression Statement: %q\n", stmt.Expression)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}
func (p *Parser) parseLetStatement() ast.Statement {

	letStmt := &ast.LetStatement{Token: p.curToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}

	letStmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	// p.peekError(token.ASSIGN)

	p.nextToken() // skip current identifier
	p.nextToken() // skip ASSIGN

	letStmt.Value = p.parseExpression(LOWEST)

	for p.curToken.Type != token.SEMICOLON {
		p.nextToken()
	}

	return letStmt
}

func (p *Parser) parseReturnStatement() ast.Statement {

	returnStmt := &ast.ReturnStatement{Token: p.curToken}

	for p.curToken.Type != token.SEMICOLON {
		p.nextToken()
	}
	return returnStmt
}

// helper
func (p *Parser) expectPeek(tt token.TokenType) bool {
	if p.peekToken.Type != tt {
		p.peekError(tt)
		return false
	} else {
		p.nextToken()
	}

	return true
}

func (p *Parser) peekTokenIs(tt token.TokenType) bool {
	return p.peekToken.Type == tt
}
func (p *Parser) curTokenIs(tt token.TokenType) bool {
	return p.curToken.Type == tt
}

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

func (p *Parser) registerPrefix(tt token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tt] = fn
}

func (p *Parser) registerInfix(tt token.TokenType, fn infixParseFn) {
	p.infixParseFns[tt] = fn
}

func (p *Parser) getPrecedence(tt token.TokenType) int {
	return p.Precedences[tt]
}

func (p *Parser) getCurrentPrecedence() int {
	fmt.Printf("Precedence of %q", p.peekToken.Type)
	return p.Precedences[p.peekToken.Type]
}
