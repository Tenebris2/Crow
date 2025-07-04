package object

import "fmt"

type Environment struct {
	env      map[string]Object
	outerEnv *Environment
}

func NewEnvironment(outer *Environment) *Environment {
	env := make(map[string]Object)
	return &Environment{env: env, outerEnv: outer}
}

func (e *Environment) Get(ident string) Object {
	value := e.env[ident]

	if value != nil {
		return value
	}

	if e.outerEnv != nil {
		value = e.outerEnv.Get(ident)
	}

	if value != nil {
		return value
	}

	return nil
}

func (e *Environment) Set(ident string, obj Object) {
	if e.env == nil {
		e.env = make(map[string]Object)
	}

	e.env[ident] = obj
}

func (e *Environment) PrintDbg() {
	fmt.Println("[")
	for k, v := range e.env {
		fmt.Printf("Key: %v, Map: %v\n", k, v)
	}
	fmt.Println("]")
}
