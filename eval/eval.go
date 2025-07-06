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
	case *ast.StringLiteral:
		return &object.String{Value: node.Value}
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

		return evalCallExpression(functionObj, args)
	case *ast.ArrayLiteral:
		elements := node.Elements
		return evalArrayExpression(elements, env)
	case *ast.IndexExpression:
		elements := Eval(node.Left, env)
		index := Eval(node.Index, env)

		return evalIndexExpression(elements, index)
	case *ast.LoopStatement:
		return evalLoopStatement(node.Condition, node.StatementBlock, env)
	default:
		return newError("Program has no more statements to parse got %s", node)
	}

}

func evalIdentifier(ident string, env *object.Environment) object.Object {
	value := env.Get(
		ident,
	)

	if value != nil {
		return value
	}

	if builtin, ok := builtins[ident]; ok {
		return builtin
	}

	return newError("identifier not found " + ident)
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
	case left.Type() == object.STRING_OBJECT && right.Type() == object.STRING_OBJECT:
		return evalStringInfixExpression(left, operator, right)
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
func evalStringInfixExpression(left object.Object, operator token.TokenType, right object.Object) object.Object {
	leftVal := left.(*object.String).Value
	rightVal := right.(*object.String).Value
	switch operator {
	case token.PLUS:
		return &object.String{Value: leftVal + rightVal}
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

	env.Set(name, value)
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

	switch function := function.(type) {
	case *object.Function:
		params := function.Parameters

		newEnv := object.NewEnvironment(function.Env)

		if len(arguments) != len(params) {
			return newError(
				"Missing argument with argument = %q and params = %q", arguments, params)
		}

		for idx, arg := range arguments {
			newEnv.Set(params[idx].Value, arg)
		}

		// newEnv.PrintDbg()
		// functionObj.Env.PrintDbg()

		evaluated := Eval(function.Body, newEnv)
		return unwrapReturnValue(evaluated)
	case *object.Builtin:
		return function.Fun(arguments...)
	default:
		return newError("not a function %s", function.Type())
	}
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

// Error handling, for creating new errors
func newError(format string, a ...interface{}) *object.Error {
	return &object.Error{Message: fmt.Sprintf(format, a...)}
}

// Error handling, for checking it is correct object type else return an ERROR_OBJECT
func isError(obj object.Object) bool {
	if obj != nil {
		return obj.Type() == object.ERROR_OBJECT
	}
	return false
}

func evalArrayExpression(elements []ast.Expression, env *object.Environment) object.Object {

	arr := &object.Array{}

	// parse elements from Expression to Object
	elementList := evalElements(elements, env)

	arr.Elements = elementList

	return arr
}

func evalElements(elements []ast.Expression, env *object.Environment) []object.Object {

	var objs []object.Object

	for _, e := range elements {
		objs = append(objs, Eval(e, env))
	}

	return objs
}

func evalIndexExpression(left, index object.Object) object.Object {
	switch left := left.(type) {
	case *object.Array:
		if index, ok := index.(*object.Integer); ok {
			return left.Elements[index.Value]
		} else {
			return newError("index type is not of type INTEGER, got type %s instead", index.Type())
		}
	case *object.String:
		if index, ok := index.(*object.Integer); ok {
			return &object.String{Value: string(left.Value[index.Value])}
		} else {
			return newError("index type is not of type INTEGER, got type %s instead", index.Type())
		}
	default:
		return newError("unsupported type: %s %s", left.Type(), index.Type())
	}
}
func unwrapReturnValue(obj object.Object) object.Object {
	if returnValue, ok := obj.(*object.ReturnValue); ok {
		return returnValue.Value
	}
	return obj
}

func evalLoopStatement(condition ast.Expression, function *ast.BlockStatement, env *object.Environment) object.Object {

	var result object.Object

	for isTruth(Eval(condition, env)) {
		result = Eval(function, env)

		if result != nil {
			rt := result.Type()
			if rt == object.RETURN_VALUE_OBJECT || rt == object.ERROR_OBJECT {
				return result
			}
		}
	}

	return result
}
