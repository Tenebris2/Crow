package token

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
}

const (
	ILLEGAL = "ILLEGAL"
	EOF     = "EOF"

	// Identifiers + literals
	IDENT = "IDENT"
	INT   = "INT"

	STRING = "STRING"

	// Operators
	ASSIGN   = "="
	PLUS     = "+"
	MINUS    = "-"
	BANG     = "!"
	ASTERISK = "*"
	SLASH    = "/"

	LT = "<"
	GT = ">"
	// Delimiters
	COMMA     = ","
	SEMICOLON = ";"

	LPAREN   = "("
	RPAREN   = ")"
	LBRACE   = "{"
	RBRACE   = "}"
	LBRACKET = "["
	RBRACKET = "]"

	// Keywords

	FUNCTION = "FUNCTION"
	LET      = "LET"

	IF     = "IF"
	TRUE   = "TRUE"
	FALSE  = "FALSE"
	ELSE   = "ELSE"
	RETURN = "RETURN"
	WHILE  = "WHILE"

	EQUAL  = "=="
	NEQUAL = "!="
)

var keywords = map[string]TokenType{
	"fun":    FUNCTION,
	"let":    LET,
	"if":     IF,
	"else":   ELSE,
	"return": RETURN,
	"true":   TRUE,
	"false":  FALSE,
	"while":  WHILE,
}

// lookup if it is a specific identification like 'fn' for function or 'let' for let else return ident like "variable_temp"
func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}

	return IDENT
}
