package object

type Environment struct {
	env map[string]Object
}

func NewEnvironment() *Environment {
	env := make(map[string]Object)
	return &Environment{env: env}
}

func (e Environment) Get(ident string) Object {
	value := e.env[ident]

	if value == nil {
		return nil
	}

	return value
}

func (e Environment) Set(ident string, obj Object) {
	if e.env == nil {
		e.env = make(map[string]Object)
	}

	e.env[ident] = obj
}
