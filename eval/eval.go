package eval

import (
	"fmt"
	"interpreter/ast"
	"interpreter/object"
	"interpreter/token"
)

var (
	NULL  = &object.Null{}
	TRUE  = &object.Boolean{Value: true}
	FALSE = &object.Boolean{Value: false}
)

func Eval(node ast.Node, env *object.Environment) object.Object {
	switch node := node.(type) {
	// statements
	case *ast.Program:
		return evalStatements(node.Statements, env)
	case *ast.FunctionLiteral:
		return evalFunctionLiteral(node.Parameters, node.Body, env)
	case *ast.LetStatement:
		name := node.Name.Value
		value := Eval(node.Value, env)

		if isError(value) {
			return value
		}

		return evalLetStatement(name, value, env)
	case *ast.ExpressionStatement:
		return Eval(node.Expression, env)
	case *ast.PrefixExpression:
		right := Eval(node.Operand, env)

		if isError(right) {
			return right
		}

		return evalPrefixExpression(node.Token.Type, right)
	case *ast.IntegerLiteral:
		return &object.Integer{Value: node.Value}
	case *ast.BooleanExpression:
		return evalBooleanExpression(node.Value)
	case *ast.InfixExpression:
		left := Eval(node.Left, env)

		if isError(left) {
			return left
		}
		right := Eval(node.Right, env)
		if isError(right) {
			return right
		}

		return evalInfixExpression(left, node.Token.Type, right)
	case *ast.ConditionalExpression:
		condition := Eval(node.Condition, env)

		if isError(condition) {
			return condition
		}

		thenBlock := node.ThenStatementBlock
		elseBlock := node.ElseStatementBlock
		return evalConditionalExpression(condition, thenBlock, elseBlock, env)
	case *ast.BlockStatement:
		fmt.Println("block statement called with current block", node.Statements)
		return evalBlockStatement(node.Statements, env)
	case *ast.ReturnStatement:
		rv := Eval(node.ReturnValue, env)

		if isError(rv) {
			return rv
		}

		return &object.ReturnValue{Value: rv}
	case *ast.Identifier:
		return evalIdentifier(node.Value, env)
	case *ast.CallExpression:
		functionObj := Eval(node.Function, env)
		if isError(functionObj) {
			return functionObj
		}

		args := evalExpressions(node.Arguments, env)

		fmt.Printf("Arguments are: [")
		for _, a := range args {
			fmt.Printf(", %v", a)
		}
		fmt.Printf("]\n")

		return evalCallExpression(functionObj, args)
	default:
		return newError("Program has no more statements to parse got %s", node)
	}

}

func evalIdentifier(ident string, env *object.Environment) object.Object {
	obj := env.Get(ident)

	if obj == nil {
		return newError("identifier not found " + ident)
	}

	return env.Get(ident)
}

func evalBlockStatement(statements []ast.Statement, env *object.Environment) object.Object {
	var result object.Object
	for _, statement := range statements {
		// todo, environment

		result = Eval(statement, env)

		if result != nil {
			rt := result.Type()
			if rt == object.RETURN_VALUE_OBJECT || rt == object.ERROR_OBJECT {
				return result
			}
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
		return newError("unknown operator: %s %s", operator, right.Type())
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
		return newError("unknown operator: -%s", right.Type())
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
	case left.Type() != right.Type():
		return newError("type mismatch: %s %s %s", left.Type(), operator, right.Type())
	default:
		return newError("unknown operator: %s %s %s", left.Type(), operator, right.Type())
	}
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
		return newError("unknown operator: %s %s %s",
			left.Type(), operator, right.Type())
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
		return newError("unknown operator: %s %s %s",
			left.Type(), operator, right.Type())
	}
}

func evalConditionalExpression(condition object.Object, thenBlock ast.Node, elseBlock ast.Node, env *object.Environment) object.Object {
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

func evalStatements(statements []ast.Statement, env *object.Environment) object.Object {
	var result object.Object

	for _, statement := range statements {
		// todo, environment
		result = Eval(statement, env)

		switch result := result.(type) {
		case *object.ReturnValue:
			return result.Value
		case *object.Error:
			return result
		}
	}

	return result
}

func evalLetStatement(name string, value object.Object, env *object.Environment) object.Object {

	if env.Get(name) == nil {
		env.Set(name, value)
	}
	return value
}

func evalFunctionLiteral(parameters []*ast.Identifier, body *ast.BlockStatement, env *object.Environment) object.Object {
	function := &object.Function{}

	// env := object.NewEnvironment()
	function.Env = env

	function.Parameters = parameters
	function.Body = body

	return function
}

func evalCallExpression(function object.Object, arguments []object.Object) object.Object {

	functionObj := function.(*object.Function)
	params := functionObj.Parameters

	newEnv := object.NewEnvironment()

	if len(arguments) != len(params) {
		return newError(
			"Missing argument with argument = %q and params = %q", arguments, params)
	}

	for idx, arg := range arguments {
		newEnv.Set(params[idx].Value, arg)
	}

	return Eval(functionObj.Body, newEnv)
}

func evalExpressions(exps []ast.Expression, env *object.Environment) []object.Object {
	var result []object.Object
	for _, e := range exps {
		evaluated := Eval(e, env)
		if isError(evaluated) {
			return []object.Object{evaluated}
		}
		result = append(result, evaluated)
	}
	return result
}

// Error handling

func newError(format string, a ...interface{}) *object.Error {
	return &object.Error{Message: fmt.Sprintf(format, a...)}
}
func isError(obj object.Object) bool {
	if obj != nil {
		return obj.Type() == object.ERROR_OBJECT
	}
	return false
}
