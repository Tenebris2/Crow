package object

import (
	"bytes"
	"crow/ast"
	"fmt"
	"hash/fnv"
	"strings"
)

const (
	INTEGER_OBJECT         = "INTEGER"
	BOOLEAN_OBJECT         = "BOOLEAN"
	NULL_OBJECT            = "NULL"
	RETURN_VALUE_OBJECT    = "RETURN"
	FUNCTION_OBJECT        = "FUNCTION"
	ERROR_OBJECT           = "ERROR"
	STRING_OBJECT          = "STRING"
	BUILTIN_OBJECT         = "BUILTIN"
	ARRAY_OBJECT           = "ARRAY"
	BREAK_SIGNAL_OBJECT    = "BREAK"
	CONTINUE_SIGNAL_OBJECT = "CONTINUE"
	MAP_OBJECT             = "MAP"
)

type HashKey struct {
	Type  ObjectType
	Value uint64
}

type Hashable interface {
	HashKey() HashKey
}
type ObjectType string
type Object interface {
	Type() ObjectType
	Inspect() string
}

type Integer struct {
	Value int64
}

func (i *Integer) Type() ObjectType {
	return INTEGER_OBJECT
}

func (i *Integer) Inspect() string {
	return fmt.Sprintf("%d", i.Value)
}

func (i *Integer) HashKey() HashKey {
	return HashKey{Type: INTEGER_OBJECT, Value: uint64(i.Value)}
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
func (b *Boolean) HashKey() HashKey {
	var value uint64

	if b.Value {
		value = 1
	} else {
		value = 0
	}

	return HashKey{Type: BOOLEAN_OBJECT, Value: value}
}

func (s *String) HashKey() HashKey {
	h := fnv.New64a()

	h.Write([]byte(s.Value))

	return HashKey{Type: STRING_OBJECT, Value: h.Sum64()}
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

type String struct {
	Value string
}

func (i *String) Type() ObjectType {
	return STRING_OBJECT
}

func (i *String) Inspect() string {
	return i.Value
}

type BuiltinFunction func(args ...Object) Object

type Builtin struct {
	Fun BuiltinFunction
}

func (b *Builtin) Type() ObjectType { return BUILTIN_OBJECT }
func (b *Builtin) Inspect() string  { return "builtin function" }

type Array struct {
	Elements []Object
}

func (a *Array) Type() ObjectType {
	return ARRAY_OBJECT
}

func (a *Array) Inspect() string {
	var out bytes.Buffer

	out.WriteString("[")
	for _, e := range a.Elements {
		out.WriteString(e.Inspect())
		out.WriteString(", ")
	}

	out.WriteString("]")
	return out.String()
}

type BreakSignal struct {
}

func (bs *BreakSignal) Type() ObjectType {
	return BREAK_SIGNAL_OBJECT
}

func (bs *BreakSignal) Inspect() string {
	return "break"
}

type ContinueSignal struct {
}

func (cs *ContinueSignal) Type() ObjectType {
	return CONTINUE_SIGNAL_OBJECT
}

func (cs *ContinueSignal) Inspect() string {
	return "continue"
}

type HashPair struct {
	Key   Object
	Value Object
}
type Map struct {
	Pairs map[HashKey]HashPair
}

func (m *Map) Type() ObjectType {
	return MAP_OBJECT
}

func (m *Map) Inspect() string {
	var out bytes.Buffer

	out.WriteString("{")
	for _, v := range m.Pairs {
		key, value := v.Key, v.Value
		out.WriteString(key.Inspect())
		out.WriteString(":")
		out.WriteString(value.Inspect())
		out.WriteString(", ")
	}

	out.WriteString("}")
	return out.String()
}
