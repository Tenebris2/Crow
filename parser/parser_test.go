package parser

import (
	"interpreter/ast"
	"interpreter/lexer"
	"interpreter/token"
	"testing"
)

func checkParserError(t *testing.T, p *Parser) {
	errors := p.Errors()

	if len(errors) == 0 {
		return
	}

	t.Errorf("parser had %d errors", len(errors))

	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}

	t.FailNow()
}

func TestLetStatement(t *testing.T) {
	input := `
let x = 5;
let y = 10;
let foobar = 838383;
`

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserError(t, p)
	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 3 {
		t.Fatalf("program.Statements does not contain 3 statements, got=%d", len(program.Statements))
	}

	tests := []struct {
		expectedIdentifer string
	}{
		{"x"}, {"y"}, {"foobar"},
	}

	for i, tt := range tests {
		stmt := program.Statements[i]
		if !testLetStatement(t, stmt, tt.expectedIdentifer) {
			return
		}
	}
}

func testLetStatement(t *testing.T, s ast.Statement, name string) bool {
	if s.TokenLiteral() != "let" {
		t.Errorf("s.TokenLiteral not 'let'. got=%q", s.TokenLiteral())
		return false
	}

	letStmt, ok := s.(*ast.LetStatement)

	if !ok {
		t.Errorf("s not *ast.LetStatement, got=%T", s)
		return false
	}

	if letStmt.Name.Value != name {
		t.Errorf("letStmt.Name.Value not '%s', got=%s", name, letStmt.Name.Value)
	}

	if letStmt.Name.TokenLiteral() != name {
		t.Errorf("letStmt.Name not '%s', got=%s", name, letStmt.Name)
	}

	return true
}

func TestReturnStatements(t *testing.T) {
	input := `
return 5;
return 10;
return 993322;
`

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()

	checkParserError(t, p)

	if len(program.Statements) != 3 {
		t.Fatalf("program.Statements does not contain 3 statements, got=%d", len(program.Statements))
	}

	for _, stmt := range program.Statements {
		returnStmt, ok := stmt.(*ast.ReturnStatement)
		if !ok {
			t.Errorf("stmt not *ast.returnStatement, got=%T", stmt)
		}

		if returnStmt.TokenLiteral() != "return" {
			t.Errorf("returnStmt.TokenLiteral not 'return', got %q", returnStmt.TokenLiteral())
		}

	}

}

func TestIdentifierExpression(t *testing.T) {
	input := "foobar;"

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserError(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d", len(program.Statements))
	}
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T",
			program.Statements[0])
	}
	ident, ok := stmt.Expression.(*ast.Identifier)
	if !ok {
		t.Fatalf("exp not *ast.Identifier. got=%T", stmt.Expression)
	}
	if ident.Value != "foobar" {
		t.Errorf("ident.Value not %s. got=%s", "foobar", ident.Value)
	}
	if ident.TokenLiteral() != "foobar" {
		t.Errorf("ident.TokenLiteral not %s. got=%s", "foobar",
			ident.TokenLiteral())
	}
}

func TestIntegerLiteralExpression(t *testing.T) {
	input := "5;"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserError(t, p)
	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d",
			len(program.Statements))
	}
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T",
			program.Statements[0])
	}
	literal, ok := stmt.Expression.(*ast.IntegerLiteral)
	if !ok {
		t.Fatalf("exp not *ast.IntegerLiteral. got=%T", stmt.Expression)
	}
	if literal.Value != 5 {
		t.Errorf("literal.Value not %d. got=%d", 5, literal.Value)
	}
	if literal.TokenLiteral() != "5" {
		t.Errorf("literal.TokenLiteral not %s. got=%s", "5",
			literal.TokenLiteral())
	}
}

func TestLetStatementAndExpression(t *testing.T) {
	input := "let foobar = a;"

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserError(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d", len(program.Statements))
	}
	stmt, ok := program.Statements[0].(*ast.LetStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.LetStatement. got=%T",
			program.Statements[0])
	}

	if stmt.Name.Value != "foobar" {
		t.Fatalf(
			"stmt.Name.Value is not foobar, got = %q", stmt.Name.Value)
	}

	stmtValue, ok := stmt.Value.(*ast.Identifier)

	if !ok {
		t.Fatalf("stmtValue is not *ast.Identifier got=%T",
			stmt.Value)
	}

	if stmtValue.Value != "a" {
		t.Fatalf("stmtValue.Value is not 'a', got = %q", stmtValue.Value)
	}
}

func TestPrefixExpression(t *testing.T) {
	input := "+-!foobar;"

	// curToken = - -> parseExpresssion -> parseprefixOperator, opereator = curToken(-) -> operand = parseExpression
	// -> parsePrefixOperator, operator = +

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserError(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)

	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T",
			program.Statements[0])
	}

	prefix_0, ok := stmt.Expression.(*ast.PrefixExpression)

	if !ok {
		t.Fatalf("expected PrefixExpression , got=%T", stmt.Expression)
	}

	operator_0 := prefix_0.Token

	if operator_0.Type != token.PLUS {
		t.Errorf("operator_0.Type expeceted to be '+'. got=%s", operator_0.Type)
	}

	prefix_1, ok := prefix_0.Operand.(*ast.PrefixExpression)

	if !ok {
		t.Fatalf("expected PrefixExpression , got=%T", prefix_0.Operand)
	}

	operator_1 := prefix_1.Token

	if operator_1.Type != token.MINUS {
		t.Errorf("operator_1.Type expeceted to be '-'. got=%s", operator_1.Type)
	}

	prefix_2, ok := prefix_1.Operand.(*ast.PrefixExpression)

	if !ok {
		t.Fatalf("expected PrefixExpression , got=%T", prefix_1.Operand)
	}

	operator_2 := prefix_2.Token

	if operator_2.Type != token.BANG {
		t.Errorf("operator_2.Type expeceted to be '-'. got=%s", operator_2.Type)
	}

	ident, ok := prefix_2.Operand.(*ast.Identifier)

	if !ok {
		t.Fatalf("exp not *ast.Identifier. got=%T", stmt.Expression)
	}

	if ident.Value != "foobar" {
		t.Errorf("ident.Value not %s. got=%s", "foobar", ident.Value)
	}

	if ident.TokenLiteral() != "foobar" {
		t.Errorf("ident.TokenLiteral not %s. got=%s", "foobar",
			ident.TokenLiteral())
	}

}
func TestInfixExpression(t *testing.T) {
	input := "foobar + barfoo;"

	// curToken = - -> parseExpresssion -> parseprefixOperator, opereator = curToken(-) -> operand = parseExpression
	// -> parsePrefixOperator, operator = +

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserError(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)

	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T",
			program.Statements[0])
	}

	infix, ok := stmt.Expression.(*ast.InfixExpression)

	if !ok {
		t.Fatalf("expected InfixExpression , got=%T", stmt.Expression)
	}

	left := infix.Left

	if left.String() != "foobar" {
		t.Errorf("left.String() expeceted to be 'foobar'. got=%s", left.String())
	}

	operator := infix.Token

	if operator.Type != token.PLUS {
		t.Errorf("operator.Type expeceted to be '+'. got=%s", operator.Type)
	}

	right := infix.Right

	if right.String() != "barfoo" {

		t.Errorf("right.String() expeceted to be 'barfoo'. got=%s", right.String())
	}
}

func TestInfixPrecedence(t *testing.T) {
	input := "foobar + barfoo * grubbox - grub * (chad + wick);"

	// curToken = - -> parseExpresssion -> parseprefixOperator, opereator = curToken(-) -> operand = parseExpression
	// -> parsePrefixOperator, operator = +

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserError(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d", len(program.Statements))
	}

	_, ok := program.Statements[0].(*ast.ExpressionStatement)

	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T",
			program.Statements[0])
	}

	programString := program.String()

	if programString != "(- (+ foobar (* barfoo grubbox)) (* grub (+ chad wick)))" {
		t.Fatalf("Got = %s", programString)
	}
}

func TestBooleanExpression(t *testing.T) {
	input := map[string]string{"true;": "true", "false;": "false"}

	for input, expected := range input {
		l := lexer.New(input)
		p := New(l)

		program := p.ParseProgram()
		checkParserError(t, p)

		if len(program.Statements) != 1 {
			t.Fatalf("program has not enough statements. got=%d", len(program.Statements))
		}

		_, ok := program.Statements[0].(*ast.ExpressionStatement)

		if !ok {
			t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T",
				program.Statements[0])
		}

		programString := program.String()

		if programString != expected {
			t.Fatalf("Expected %s, Got = %s", expected, programString)
		}

	}
}

func TestConditionalStatement(t *testing.T) {
	input :=
		`if a {
  let x = 5;
} else {
  let x = 6;
}
`

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserError(t, p)
	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	stmt := program.Statements[0]

	if stmt.String() != "IF (a) THEN {let x = 5; } ELSE {let x = 6; }" {
		t.Fatalf("Expected, got = %v", stmt.String())
	}
}

func TestCallExpression(t *testing.T) {
	input := "call(a, b, c);"

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserError(t, p)
	//
	// if len(program.Statements) != 1 {
	// 	t.Fatalf("program has not enough statements. got=%d", len(program.Statements))
	// }
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)

	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T",
			program.Statements[0])
	}

	callExpression := stmt.Expression

	if callExpression.String() != "CALL call(a, b, c, )" {
		t.Fatalf("Got %q", callExpression.String())
	}
}

func TestParseNestedIfExpression(t *testing.T) {
	input := `
  if true {
    if true {
      return 1
    }

    return 2
  }`

	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserError(t, p)

	if program.String() != "IF (true) THEN {IF (true) THEN {return 1; }; return 2; }" {
		t.Fatalf(program.String())
	}
}
