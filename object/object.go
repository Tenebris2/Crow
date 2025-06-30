package object

import (
	"bytes"
	"fmt"
	"interpreter/ast"
	"strings"
)

type ObjectType string
type Object interface {
	Type() ObjectType
	Inspect() string
}

type Integer struct {
	Value int64
}

const (
	INTEGER_OBJECT      = "INTEGER"
	BOOLEAN_OBJECT      = "BOOLEAN"
	NULL_OBJECT         = "NULL"
	RETURN_VALUE_OBJECT = "RETURN"
	FUNCTION_OBJECT     = "FUNCTION"
	ERROR_OBJECT        = "ERROR"
)

func (i *Integer) Type() ObjectType {
	return INTEGER_OBJECT
}

func (i *Integer) Inspect() string {
	return fmt.Sprintf("%d", i.Value)
}

type Boolean struct {
	Value bool
}

func (b *Boolean) Type() ObjectType {
	return BOOLEAN_OBJECT
}

func (b *Boolean) Inspect() string {
	return fmt.Sprintf("%t", b.Value)
}

type Null struct {
}

func (n *Null) Type() ObjectType {
	return NULL_OBJECT
}

func (n *Null) Inspect() string {
	return "null"
}

type ReturnValue struct {
	Value Object
}

func (rv *ReturnValue) Type() ObjectType {
	return RETURN_VALUE_OBJECT
}

func (rv *ReturnValue) Inspect() string {
	return rv.Value.Inspect()
}

type Function struct {
	Parameters []*ast.Identifier
	Body       *ast.BlockStatement
	Env        *Environment
}

func (f *Function) Type() ObjectType {
	return FUNCTION_OBJECT
}

func (f *Function) Inspect() string {
	var out bytes.Buffer
	params := []string{}

	for _, p := range f.Parameters {
		params = append(params, p.String())
	}

	out.WriteString("fun(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(")")
	out.WriteString(f.Body.String())

	return out.String()
}

type Error struct {
	Message string
}

func (e *Error) Type() ObjectType {
	return ERROR_OBJECT
}

func (e *Error) Inspect() string {
	return "ERROR: " + e.Message
}
