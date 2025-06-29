package eval

import (
	"interpreter/ast"
	"interpreter/environment"
	"interpreter/object"
	"interpreter/token"
)

var (
	NULL  = &object.Null{}
	TRUE  = &object.Boolean{Value: true}
	FALSE = &object.Boolean{Value: false}
)

func Eval(node ast.Node, env environment.Environment) object.Object {
	switch node := node.(type) {
	// statements
	case *ast.Program:
		return evalStatements(node.Statements, env)
	case *ast.LetStatement:
		name := node.Name.Value
		value := Eval(node.Value, env)
		return evalLetStatement(name, value, env)
	case *ast.ExpressionStatement:
		return Eval(node.Expression, env)
	case *ast.PrefixExpression:
		right := Eval(node.Operand, env)
		return evalPrefixExpression(node.Token.Type, right)
	case *ast.IntegerLiteral:
		return &object.Integer{Value: node.Value}
	case *ast.BooleanExpression:
		return evalBooleanExpression(node.Value)
	case *ast.InfixExpression:
		left := Eval(node.Left, env)
		right := Eval(node.Right, env)
		return evalInfixExpression(left, node.Token.Type, right)
	case *ast.ConditionalExpression:
		condition := Eval(node.Condition, env)
		thenBlock := node.ThenStatementBlock
		elseBlock := node.ElseStatementBlock
		return evalConditionalExpression(condition, thenBlock, elseBlock, env)
	case *ast.BlockStatement:
		return evalBlockStatement(node.Statements, env)
	case *ast.ReturnStatement:
		rv := Eval(node.ReturnValue, env)
		return &object.ReturnValue{Value: rv}
	case *ast.Identifier:
		return evalIdentifier(node.Value, env)
	}

	return nil
}

func evalIdentifier(ident string, env environment.Environment) object.Object {
	return env.Get(ident)
}

func evalBlockStatement(statements []ast.Statement, env environment.Environment) object.Object {
	var result object.Object
	for _, statement := range statements {
		// todo, environment

		result = Eval(statement, env)

		if result != nil && result.Type() == object.RETURN_VALUE_OBJECT {
			return result
		}
	}

	return result
}

func evalPrefixExpression(operator token.TokenType, right object.Object) object.Object {
	switch operator {
	case token.BANG:
		return evalBangPrefixExpression(right)
	case token.MINUS:
		return evalMinusPrefixExpression(right)
	default:
		return NULL
	}
}

func evalBangPrefixExpression(right object.Object) object.Object {
	switch right {
	case TRUE:
		return FALSE
	case FALSE:
		return TRUE
	case NULL:
		return TRUE
	default:
		return FALSE
	}
}

func evalBooleanExpression(val bool) object.Object {
	return naiveBoolToBoolean(val)
}

func naiveBoolToBoolean(val bool) object.Object {
	if val {
		return TRUE
	} else {
		return FALSE
	}
}

func evalMinusPrefixExpression(right object.Object) object.Object {
	val, ok := right.(*object.Integer)

	if !ok {
		return NULL
	}

	obj := &object.Integer{Value: -val.Value}

	return obj
}

func evalInfixExpression(left object.Object, operator token.TokenType, right object.Object) object.Object {
	switch {
	case left.Type() == object.INTEGER_OBJECT && right.Type() == object.INTEGER_OBJECT:
		return evalIntegerInfixExpression(left, operator, right)
	case left.Type() == object.BOOLEAN_OBJECT && right.Type() == object.BOOLEAN_OBJECT:
		return evalBooleanInfixExpression(left, operator, right)
	}

	return NULL
}

func evalIntegerInfixExpression(left object.Object, operator token.TokenType, right object.Object) object.Object {
	leftVal := left.(*object.Integer).Value
	rightVal := right.(*object.Integer).Value
	switch operator {
	case token.PLUS:
		return &object.Integer{Value: leftVal + rightVal}
	case token.MINUS:
		return &object.Integer{Value: leftVal - rightVal}
	case token.ASTERISK:
		return &object.Integer{Value: leftVal * rightVal}
	case token.SLASH:
		return &object.Integer{Value: leftVal / rightVal}
	case token.EQUAL:
		return naiveBoolToBoolean(leftVal == rightVal)
	case token.NEQUAL:
		return naiveBoolToBoolean(leftVal != rightVal)
	case token.LT:
		return naiveBoolToBoolean(leftVal < rightVal)
	case token.GT:
		return naiveBoolToBoolean(leftVal > rightVal)
	default:
		return NULL
	}
}

func evalBooleanInfixExpression(left object.Object, operator token.TokenType, right object.Object) object.Object {
	leftVal := left.(*object.Boolean).Value
	rightVal := right.(*object.Boolean).Value
	switch operator {
	case token.EQUAL:
		return naiveBoolToBoolean(leftVal == rightVal)
	case token.NEQUAL:
		return naiveBoolToBoolean(leftVal != rightVal)
	default:
		return NULL
	}
}

func evalConditionalExpression(condition object.Object, thenBlock ast.Node, elseBlock ast.Node, env environment.Environment) object.Object {
	if isTruth(condition) {
		return Eval(thenBlock, env)
	}

	eb, ok := elseBlock.(*ast.BlockStatement)

	if eb == nil || !ok {
		return NULL
	}

	return Eval(elseBlock, env)
}

func isTruth(condition object.Object) bool {
	switch condition {
	case TRUE:
		return true
	case FALSE:
		return false
	case NULL:
		return false
	default:
		return true
	}
}

func evalStatements(statements []ast.Statement, env environment.Environment) object.Object {
	var result object.Object
	for _, statement := range statements {
		// todo, environment

		// nhap:
		// if true {
		// 	if true {
		// 		return 2
		// 	}
		//
		// 	return 1
		// }
		// final res: 1 -> current implementation

		result = Eval(statement, env)

		if returnValue, ok := result.(*object.ReturnValue); ok {
			return returnValue.Value
		}
	}

	return result
}

func evalLetStatement(name string, value object.Object, env environment.Environment) object.Object {
	if env.Get(name) == nil {
		env.Set(name, value)
	}

	return value
}
