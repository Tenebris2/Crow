package eval

import "interpreter/object"

var builtins = map[string]*object.Builtin{
	"len": &object.Builtin{
		Fun: func(args ...object.Object) object.Object {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%v, want=%v", len(args), 1)
			}

			switch arg := args[0].(type) {
			case *object.String:
				strLen := int64(len(arg.Value))

				return &object.Integer{Value: strLen}
			default:
				return newError("argument to `len` not supported, got %s",
					args[0].Type())
			}
		},
	},
}
