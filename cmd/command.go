package cmd

import (
	"hotel/hotel/logic"
)

type Command struct {
	Cmd         	CommandType
	Reservation 	logic.Reservation
	ReturnContent 	chan string
}
