package cmd

import (
	"hotel/hotel/logic"
)

// Command represents a command sent by a client after parsing
type Command struct {
	Cmd         	CommandType
	Reservation 	logic.Reservation
	ReturnContent 	chan string
}
