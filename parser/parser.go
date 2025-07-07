package parser

import (
	"crow/ast"
	"crow/lexer"
	"crow/logger"
	"crow/token"
	"fmt"
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
	ASSIGN
	INDEX // []
	CALL  // myFunction(x)
	// LOWEST
	// EQUALS
	// LESSGREATER
)

var lg = logger.GetInstance()

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
	p.registerPrefix(token.STRING, p.parseStringLiteral)
	p.registerPrefix(token.TRUE, p.parseBooleanExpression)
	p.registerPrefix(token.FALSE, p.parseBooleanExpression)
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(token.IF, p.parseConditionalExpression)
	p.registerPrefix(token.FUNCTION, p.parseFunctionExpression)
	p.registerPrefix(token.LBRACKET, p.parseArrayExpression)

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
	p.registerInfix(token.LBRACKET, p.parseIndexExpression)
	p.registerInfix(token.ASSIGN, p.parseAssignExpression)

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
		token.LBRACKET: INDEX,
		token.ASSIGN:   ASSIGN,
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

func (p *Parser) reportError(msg string, tt ...token.TokenType) {
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
func (p *Parser) parseStringLiteral() ast.Expression {
	lit := &ast.StringLiteral{Token: p.curToken}

	lit.Value = p.curToken.Literal

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
	case token.FOR:
		return p.parseLoopStatement()
	case token.SEMICOLON:
		return nil
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

	fmt.Println("Parsing prefix: ", leftExp)
	// precedence example:
	// a + b * c;

	// prefix identifier a, infix, left = a, operator = +, now at b,

	for !p.curTokenIs(token.SEMICOLON) && precedence < p.getCurrentPrecedence() {

		peekExp := p.peekToken

		infix := p.infixParseFns[peekExp.Type]

		if infix == nil {
			return leftExp
		}

		fmt.Println("Parsing infix of type ", peekExp.Type)
		p.nextToken()

		leftExp = infix(leftExp)
	}

	return leftExp
}
func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.curToken}

	stmt.Expression = p.parseExpression(LOWEST)

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

	// if !p.expectPeek(token.SEMICOLON) && !p.expectPeek(token.EOF) {
	// 	fmt.Println("Expecting Semicolon is getting", p.curToken)
	// 	return nil
	// }

	return letStmt
}

func (p *Parser) parseReturnStatement() ast.Statement {

	returnStmt := &ast.ReturnStatement{Token: p.curToken}

	p.nextToken()

	returnStmt.ReturnValue = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	// if !p.peekTokenIs(token.EOF) {
	// 	p.nextToken()
	// }

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
func (p *Parser) parseLoopStatement() ast.Statement {
	// if CONDITION_EXPRESSION { THEN_BLOCK_STATEMENT } ELSE ELSE_BLOCK_STATEMENT
	loopStatement := &ast.LoopStatement{Token: p.curToken} // Token = FOR

	// consume token FOR
	p.nextToken()

	condition := p.parseExpression(LOWEST)

	loopStatement.Condition = condition

	// move to { to parse BLOCK_STATEMENT
	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	// parse THEN BLOCK STATEMENT

	block := p.parseBlockStatement()

	loopStatement.StatementBlock = block

	return loopStatement
}

func (p *Parser) parseConditionalExpression() ast.Expression {
	// if CONDITION_EXPRESSION { THEN_BLOCK_STATEMENT } ELSE ELSE_BLOCK_STATEMENT
	conditionalExp := &ast.ConditionalExpression{Token: p.curToken} // Token = IF

	// parse condition expression
	p.nextToken()

	condition := p.parseExpression(LOWEST)

	conditionalExp.Condition = condition

	// move to { to parse BLOCK_STATEMENT
	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	// parse THEN BLOCK STATEMENT

	thenStmt := p.parseBlockStatement()

	conditionalExp.ThenStatementBlock = thenStmt

	if p.peekTokenIs(token.ELSE) {
		p.nextToken()
		if !p.expectPeek(token.LBRACE) {

			return nil
		}

		elseStmt := p.parseBlockStatement()

		conditionalExp.ElseStatementBlock = elseStmt

	}

	return conditionalExp
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	blockStmt := &ast.BlockStatement{}

	blockStmt.Token = p.curToken

	// stmt := p.parseStatement()

	// if stmt != nil {
	// 	fmt.Printf("Statement parsing is %v\n", stmt)
	// 	statements = append(statements, stmt)
	// }

	p.nextToken()

	for !p.curTokenIs(token.RBRACE) && !p.curTokenIs(token.EOF) {

		stmt := p.parseStatement()

		if stmt != nil {
			blockStmt.Statements = append(blockStmt.Statements, stmt)
		}

		p.nextToken()
	}

	return blockStmt
}

func (p *Parser) parseCallExpression(left ast.Expression) ast.Expression {

	callExp := &ast.CallExpression{Token: p.curToken, Function: left}

	callExp.Arguments = p.parseFunctionArguments()

	return callExp
}

func (p *Parser) parseFunctionArguments() []ast.Expression {
	return p.parseExpressionList(token.RPAREN)
}

func (p *Parser) parseFunctionParameters() []*ast.Identifier {
	args := []*ast.Identifier{}

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return args
	}

	p.nextToken() // skip to identifier
	exp := p.parseExpression(LOWEST)

	id, ok := exp.(*ast.Identifier)

	if !ok {
		p.reportError(fmt.Sprintf("Expected next token to be Identifier, got %s", exp))
		return nil
	}

	args = append(args, id)

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()

		exp := p.parseExpression(LOWEST)

		id, ok := exp.(*ast.Identifier)

		if !ok {
			p.reportError(fmt.Sprintf("Expected next token to be Identifier, got %s", exp))
			return nil
		}

		args = append(args, id)

	}

	p.nextToken() // consume LPAREN

	return args
}
func (p *Parser) parseFunctionExpression() ast.Expression {
	function := &ast.FunctionLiteral{Token: p.curToken}

	p.nextToken() // skip to

	function.Parameters = p.parseFunctionParameters()

	p.nextToken() // move to {

	function.Body = p.parseBlockStatement()

	return function
}

func (p *Parser) parseArrayExpression() ast.Expression {
	arr := &ast.ArrayLiteral{Token: p.curToken}

	arr.Elements = p.parseExpressionList(token.RBRACKET)

	return arr
}

// parse elements in the array and return those elements a list of expressions
func (p *Parser) parseExpressionList(tt token.TokenType) []ast.Expression {
	elements := []ast.Expression{}

	if p.peekTokenIs(tt) {
		p.nextToken()
		return elements
	}

	p.nextToken()

	elements = append(elements, p.parseExpression(LOWEST))

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		exp := p.parseExpression(LOWEST)

		elements = append(elements, exp)
	}

	p.nextToken()

	return elements

}

func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	infix := &ast.IndexExpression{Left: left}
	infix.Token = p.curToken // LBRACKET

	p.nextToken() // skip to expression

	right := p.parseExpression(p.getPrecedence(infix.Token.Type))

	p.nextToken() // skip past the RBRACKET

	infix.Index = right

	return infix
}

func (p *Parser) parseAssignExpression(left ast.Expression) ast.Expression {
	assignedStmt := &ast.AssignExpression{Token: p.curToken}

	fmt.Println("Parsing assign expression")

	assignedStmt.Identifier = left

	fmt.Println("CALLING", p.curToken, left)

	p.nextToken() // skip to identifier

	assignedStmt.AssignedValue = p.parseExpression(ASSIGN)

	return assignedStmt
}
