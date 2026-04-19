package logger

type Logger struct {
	mode bool
	logs []string
}

var (
	instance *Logger
)

// GetInstance returns the singleton instance
func GetInstance() *Logger {
	if instance == nil {
		instance = &Logger{}
	}

	return instance
}

func (l *Logger) Log(msg string) {
	l.logs = append(l.logs, msg)
}

func (l *Logger) Logs() []string {
	return l.logs
}

func (l *Logger) setMode(mode bool) {
	l.mode = mode
}
