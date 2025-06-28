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
	EQUALS      // ==
	LESSGREATER // < or >
	SUM         // +
	PRODUCT     // *
	EXPONENT    // ^
	PREFIX      // -X or !X
	CALL        // myFunction(x)
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
	p.registerPrefix(token.TRUE, p.parseBooleanExpression)
	p.registerPrefix(token.FALSE, p.parseBooleanExpression)
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(token.IF, p.parseConditionalExpression)

	p.registerPrefixOperators(token.PLUS)
	p.registerPrefixOperators(token.MINUS)
	p.registerPrefixOperators(token.BANG)
	p.registerPrefixOperators(token.ASTERISK)

	p.infixParseFns = make(map[token.TokenType]infixParseFn)

	// infix

	p.registerInfixOperators(token.PLUS)
	p.registerInfixOperators(token.ASTERISK)
	p.registerInfixOperators(token.SLASH)
	p.registerInfixOperators(token.MINUS)
	p.registerInfixOperators(token.BANG)
	p.registerInfixOperators(token.GT)
	p.registerInfixOperators(token.LT)
	p.registerInfixOperators(token.EQUAL)
	p.registerInfixOperators(token.NEQUAL)

	p.registerInfix(token.LPAREN, p.parseCallExpression)

	// precedence

	p.Precedences = map[token.TokenType]int{
		token.PLUS:     SUM,
		token.MINUS:    SUM,
		token.ASTERISK: PRODUCT,
		token.SLASH:    PRODUCT,
		token.BANG:     PREFIX,
		token.EQUAL:    EQUALS,
		token.NEQUAL:   EQUALS,
		token.GT:       LESSGREATER,
		token.LT:       LESSGREATER,
		token.LPAREN:   CALL,
		token.COMMA:    CALL,
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

	fmt.Printf("Prefix, Operator: %v, Operand: %v\n", operator, operand)
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
	case token.LBRACE:
		return p.parseBlockStatement()
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

	for !p.curTokenIs(token.SEMICOLON) && precedence < p.getCurrentPrecedence() {

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

	fmt.Printf("Parsing Expression Statement: %v %v\n", stmt.Expression, p.curToken.Type)

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

	if !p.expectPeek(token.SEMICOLON) {
		return nil
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
	return p.Precedences[p.peekToken.Type]
}

func (p *Parser) parseBooleanExpression() ast.Expression {
	return &ast.BooleanExpression{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()

	exp := p.parseExpression(LOWEST)

	if !p.peekTokenIs(token.RPAREN) {
		return nil
	}

	p.nextToken() // consume RPAREN

	return exp
}

func (p *Parser) parseConditionalExpression() ast.Expression {
	// if CONDITION_EXPRESSION { THEN_BLOCK_STATEMENT } ELSE ELSE_BLOCK_STATEMENT
	conditionalExp := &ast.ConditionalExpression{Token: p.curToken} // Token = IF

	// parse condition expression
	p.nextToken()

	condition := p.parseExpression(LOWEST)

	conditionalExp.Condition = condition

	p.nextToken() // move to { to parse BLOCK_STATEMENT

	fmt.Printf("Current token is %s (0)\n", p.curToken.Type)

	// parse THEN BLOCK STATEMENT

	thenStmt := p.parseBlockStatement()

	conditionalExp.ThenStatementBlock = thenStmt

	if !p.expectPeek(token.ELSE) {
		return conditionalExp
	}

	p.nextToken() // move to the else statement

	fmt.Printf("Current token is %s (1)\n", p.curToken.Type)

	elseStmt := p.parseBlockStatement()

	conditionalExp.ElseStatementBlock = elseStmt

	return conditionalExp
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	p.nextToken()

	blockStmt := &ast.BlockStatement{}

	blockStmt.Token = p.curToken

	statements := []ast.Statement{}

	for !p.curTokenIs(token.RBRACE) {
		stmt := p.parseStatement()

		if stmt != nil {
			fmt.Printf("Statement parsing is %v\n", stmt)
			statements = append(statements, stmt)
		}

		p.nextToken()
	}

	blockStmt.Statements = statements

	fmt.Printf("statements are %v\n", statements)

	return blockStmt
}

func (p *Parser) parseCallExpression(left ast.Expression) ast.Expression {
	callExp := &ast.CallExpression{Token: p.curToken, Function: left}

	p.nextToken()

	callExp.Arguments = p.parseFunctionArguments()

	return callExp
}

func (p *Parser) parseFunctionArguments() []ast.Expression {
	args := []ast.Expression{}

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return args
	}

	for p.peekTokenIs(token.COMMA) {
		exp := p.parseExpression(LOWEST)

		args = append(args, exp)

		fmt.Println(args, "123321332")

		p.nextToken()
		p.nextToken()
	}

	if !p.peekTokenIs(token.LPAREN) {
		args = append(args, p.parseExpression(LOWEST))
	}

	p.nextToken() // consume LPAREN

	return args
}
