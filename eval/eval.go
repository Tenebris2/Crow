package eval

import (
	"interpreter/ast"
	"interpreter/object"
)

func Eval(node ast.Node) object.Object {
	switch node := node.(type) {
	// statements
	case *ast.Program:
		return evalStatements(node.Statements)
	case *ast.LetStatement:
		return Eval(node.Value)
	case *ast.ExpressionStatement:
		return Eval(node.Expression)
		//
	case *ast.PrefixExpression:
		return Eval(node.Operand)
	case *ast.IntegerLiteral:
		return &object.Integer{Value: node.Value}
	case *ast.BooleanExpression:
		return &object.Boolean{Value: node.Value}
	default:
		return nil
	}
}

func evalStatements(statements []ast.Statement) object.Object {
	var result object.Object
	for _, statement := range statements {
		// todo, environment
		result = Eval(statement)
	}

	return result
}
