// Package cmd contains all the logic related to user commands
package cmd

import (
	"hotel/server/logic"
)

// Command represents a command sent by a client after parsing
type Command struct {
	Cmd         	CommandType
	Reservation 	logic.Reservation
	ReturnContent 	chan string
}
