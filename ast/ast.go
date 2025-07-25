package ast

import (
	"bytes"
	"crow/token"
	"strings"
)

type Node interface {
	TokenLiteral() string
	String() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Program struct {
	Statements []Statement
}

// Program Token Literal, implementation of Node interface
func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	} else {
		return ""
	}
}

func (p *Program) String() string {
	// uses buffer instead of string concatenation because it makes a copy of a string each time it concatenates
	var out bytes.Buffer

	for _, s := range p.Statements {
		out.WriteString(s.String())
	}

	return out.String()
}

// Let Statements

type LetStatement struct {
	Token token.Token
	Name  *Identifier
	Value Expression
}

func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }

func (ls *LetStatement) statementNode() {

}

func (ls *LetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.String())
	out.WriteString(" = ")

	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}

	return out.String()
}

// Return Statement

type ReturnStatement struct {
	Token       token.Token // return token
	ReturnValue Expression
}

func (rs *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString(rs.TokenLiteral() + " ")

	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.String())
	}

	return out.String()
}

func (rs *ReturnStatement) statementNode() {

}

func (rs *ReturnStatement) TokenLiteral() string {
	return rs.Token.Literal
}

// Expression Statement

type ExpressionStatement struct {
	Token      token.Token
	Expression Expression
}

func (es *ExpressionStatement) statementNode() {

}

func (es *ExpressionStatement) TokenLiteral() string {
	return es.Token.Literal
}

func (es *ExpressionStatement) String() string {

	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

// Identifier

type Identifier struct {
	Token token.Token
	Value string
}

func (i *Identifier) expressionNode() {}
func (i *Identifier) TokenLiteral() string {
	return i.Token.Literal
}
func (i *Identifier) String() string {
	return i.Value
}

type IntegerLiteral struct {
	Token token.Token
	Value int64
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) String() string       { return il.Token.Literal }

type PrefixExpression struct {
	Token   token.Token // Operator like +, -, !
	Operand Expression
}

func (pe *PrefixExpression) TokenLiteral() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(pe.Token.Literal)
	out.WriteString(" ")
	out.WriteString(pe.Operand.String())
	out.WriteString(")")
	return out.String()
}
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(pe.Token.Literal)
	out.WriteString(" ")
	out.WriteString(pe.Operand.String())
	out.WriteString(")")
	return out.String()
}
func (pe *PrefixExpression) expressionNode() {}

type InfixExpression struct {
	Left  Expression
	Token token.Token
	Right Expression
}

func (ie *InfixExpression) TokenLiteral() string {
	return ie.Left.TokenLiteral() + ie.Token.Literal + ie.Right.TokenLiteral()
}
func (ie *InfixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(ie.Token.Literal)
	out.WriteString(" ")
	out.WriteString(ie.Left.String())
	out.WriteString(" ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")

	return out.String()
}
func (ie *InfixExpression) expressionNode() {}

// boolean expression

type BooleanExpression struct {
	Token token.Token
	Value bool
}

func (be *BooleanExpression) expressionNode() {

}

func (be *BooleanExpression) TokenLiteral() string {
	return be.Token.Literal
}
func (be *BooleanExpression) String() string {
	return be.Token.Literal
}

// Conditional Expression

type ConditionalExpression struct {
	Token              token.Token // type: IF
	Condition          Expression
	ThenStatementBlock *BlockStatement
	ElseStatementBlock *BlockStatement
}

type BlockStatement struct {
	Token      token.Token // type: {
	Statements []Statement
}

func (bs *BlockStatement) statementNode() {

}

func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }
func (bs *BlockStatement) String() string {
	var out bytes.Buffer
	out.WriteString("{")
	for _, stmt := range bs.Statements {
		out.WriteString(stmt.String())
		out.WriteString("; ")
	}
	out.WriteString("}")
	return out.String()
}
func (ce *ConditionalExpression) expressionNode() {

}

func (ce *ConditionalExpression) TokenLiteral() string {
	var out bytes.Buffer
	out.WriteString("IF (")
	out.WriteString(ce.Condition.String())
	out.WriteString(") THEN ")
	out.WriteString(ce.ThenStatementBlock.String())
	if ce.ElseStatementBlock != nil {
		out.WriteString(" ELSE ")
		out.WriteString(ce.ElseStatementBlock.String())
	}
	return out.String()
}
func (ce *ConditionalExpression) String() string {
	return ce.TokenLiteral()
}

// Call Expression

type CallExpression struct {
	Token     token.Token
	Function  Expression
	Arguments []Expression
}

func (ce *CallExpression) expressionNode() {

}
func (ce *CallExpression) TokenLiteral() string {
	return ce.Token.Literal

}
func (ce *CallExpression) String() string {
	var out bytes.Buffer
	out.WriteString("CALL ")
	out.WriteString(ce.Function.String())
	out.WriteString("(")
	for _, arg := range ce.Arguments {
		out.WriteString(arg.String())
		out.WriteString(", ")
	}
	out.WriteString(")")
	return out.String()
}

type FunctionLiteral struct {
	Token      token.Token
	Parameters []*Identifier
	Body       *BlockStatement
}

func (fl *FunctionLiteral) expressionNode() {

}
func (fl *FunctionLiteral) TokenLiteral() string {
	return fl.Token.Literal
}
func (fl *FunctionLiteral) String() string {
	var out bytes.Buffer
	params := []string{}

	for _, p := range fl.Parameters {
		params = append(params, p.String())
	}

	out.WriteString("fun(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(")")
	out.WriteString(fl.Body.String())

	return out.String()
}

type StringLiteral struct {
	Token token.Token
	Value string
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) String() string       { return sl.Token.Literal }

type ArrayLiteral struct {
	Token    token.Token
	Elements []Expression
}

func (al *ArrayLiteral) expressionNode()      {}
func (al *ArrayLiteral) TokenLiteral() string { return al.Token.Literal }
func (al *ArrayLiteral) String() string {
	var out bytes.Buffer

	out.WriteString("[")
	for _, e := range al.Elements {
		out.WriteString(e.String())
		out.WriteString(", ")
	}

	out.WriteString("]")
	return out.String()

}

// diff from infix expression naming-wise btw
type IndexExpression struct {
	Token token.Token
	Left  Expression
	Index Expression
}

func (ie *IndexExpression) expressionNode()      {}
func (ie *IndexExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IndexExpression) String() string {
	var out bytes.Buffer

	out.WriteString(ie.Left.String())
	out.WriteString("[")
	out.WriteString(ie.Index.String())
	out.WriteString("]")

	return out.String()
}

type LoopStatement struct {
	Token          token.Token // type: WHILE
	Condition      Expression
	StatementBlock *BlockStatement
}

func (ls *LoopStatement) statementNode()       {}
func (ls *LoopStatement) TokenLiteral() string { return ls.Token.Literal }
func (ls *LoopStatement) String() string {
	var out bytes.Buffer

	out.WriteString("while ")
	out.WriteString(ls.Condition.String())
	out.WriteString(" ")
	out.WriteString(ls.StatementBlock.String())

	return out.String()
}

type AssignExpression struct {
	Token         token.Token // type: IDENT
	Identifier    Expression
	AssignedValue Expression
}

func (ls *AssignExpression) expressionNode()      {}
func (ls *AssignExpression) TokenLiteral() string { return ls.Token.Literal }
func (ls *AssignExpression) String() string {
	var out bytes.Buffer

	out.WriteString(ls.Identifier.String())
	out.WriteString(" = ")
	out.WriteString(ls.AssignedValue.String())

	return out.String()
}

type ForStatement struct {
	Token          token.Token
	Init           Statement
	Condition      Expression
	Post           Statement
	StatementBlock *BlockStatement
}

func (fs *ForStatement) statementNode()       {}
func (fs *ForStatement) TokenLiteral() string { return fs.Token.Literal }
func (fs *ForStatement) String() string {
	var out bytes.Buffer

	out.WriteString("for ")
	out.WriteString(fs.Init.String())
	out.WriteString(";")
	out.WriteString(fs.Condition.String())
	out.WriteString(";")
	out.WriteString(fs.Post.String())
	out.WriteString(fs.StatementBlock.String())

	return out.String()
}

type ControlFlowSignalStatement struct {
	Token token.Token
}

func (cf *ControlFlowSignalStatement) statementNode()       {}
func (cf *ControlFlowSignalStatement) TokenLiteral() string { return cf.Token.Literal }
func (cf *ControlFlowSignalStatement) String() string {
	return cf.Token.Literal
}

type MapLiteral struct {
	Token token.Token // {
	Map   map[Expression]Expression
}

func (ml *MapLiteral) expressionNode()      {}
func (ml *MapLiteral) TokenLiteral() string { return ml.Token.Literal }
func (ml *MapLiteral) String() string {
	// let a = map[int]
	var out bytes.Buffer

	out.WriteString("{")
	for k, v := range ml.Map {
		out.WriteString(k.String())
		out.WriteString(":")
		out.WriteString(v.String())
		out.WriteString(", ")
	}

	out.WriteString("}")
	return out.String()

}
