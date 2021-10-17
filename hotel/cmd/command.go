package cmd

import "hotel/logic"

type Command struct {
	Cmd         	CommandType
	Reservation 	logic.Reservation
	ReturnContent 	chan string
}
