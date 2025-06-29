package environment

import (
	"interpreter/object"
)

type Environment struct {
	env map[string]object.Object
}

func NewEnvironment() *Environment {
	env := make(map[string]object.Object)
	return &Environment{env: env}
}

func (e *Environment) Get(ident string) object.Object {
	value := e.env[ident]

	if value == nil {
		return nil
	}
	return value
}

func (e *Environment) Set(ident string, obj object.Object) {
	if e.env == nil {
		e.env = make(map[string]object.Object)
	}

	e.env[ident] = obj
}
