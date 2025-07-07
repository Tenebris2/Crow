package main

import (
	"crow/eval"
	"crow/lexer"
	"crow/object"
	"crow/parser"
	"crow/repl"
	"fmt"
	"os"
	"os/user"
)

func main() {
	user, err := user.Current()
	if err != nil {
		panic(err)
	}

	args := os.Args

	if len(args) <= 1 {
		panic("Not enough arguments")
	}

	input := args[1]

	switch input {
	case "-i":
		fmt.Printf("Hello %s! This is Crow programming language!\n", user.Username)
		repl.Start(os.Stdin, os.Stdout)
	default:
		data, err := os.ReadFile(input)

		if err != nil {
			panic(err)
		}

		env := object.NewEnvironment(nil)
		l := lexer.New(string(data))
		p := parser.New(l)

		program := p.ParseProgram()
		for _, msg := range p.Errors() {
			fmt.Println(msg)
		}

		eval.Eval(program, env)
	}

}
