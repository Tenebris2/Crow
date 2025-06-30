package repl

import (
	"bufio"
	"fmt"
	"interpreter/eval"
	"interpreter/lexer"
	"interpreter/object"
	"interpreter/parser"
	"io"
)

const LOG_MODE = false
const PROMPT = ">> "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	env := object.NewEnvironment()

	for {
		fmt.Printf(PROMPT)
		scanned := scanner.Scan()

		if !scanned {
			return
		}

		line := scanner.Text()

		if line == "" {
			continue
		}
		l := lexer.New(line)
		p := parser.New(l)

		program := p.ParseProgram()

		errors := p.Errors()

		if len(errors) != 0 {
			fmt.Printf("Parser had %d errors\n", len(errors))
		}

		for _, msg := range errors {
			fmt.Printf("parser error: %q\n", msg)
		}

		fmt.Printf("%v\n", eval.Eval(program, env).Inspect())
	}
}
