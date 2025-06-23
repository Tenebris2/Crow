package parser

import (
	"interpreter/ast"
	"interpreter/lexer"
	"interpreter/token"
)

type Parser struct {
	l *lexer.Lexer

	curToken  token.Token
	peekToken token.Token
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l}

	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for p.curToken.Type != token.EOF {
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
	default:
		return nil
	}
}

func (p *Parser) parseLetStatement() ast.Statement {

	letStmt := &ast.LetStatement{Token: p.curToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}

	letStmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	for p.curToken.Type != token.SEMICOLON {
		p.nextToken()
	}
	return letStmt
}

func (p *Parser) expectPeek(tt token.TokenType) bool {
	if p.peekToken.Type != tt {
		return false
	} else {
		p.nextToken()
	}

	return true
}
