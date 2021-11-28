// Package cmd contains all the logic related to user commands
package cmd

import (
	"hotel/server/logic"
	"strconv"
)

// Command represents a command sent by a client after parsing
type Command struct {
	Cmd           CommandType
	Reservation   logic.Reservation
	ReturnContent chan string
}

type SyncCommand struct {
	AuthorId int
	Command Command
}

func (c *Command) ToString() string {
	return string(c.Cmd) + " " + c.Reservation.ToString()
}

func (c *Command) ToSyncStringCommand(idSender int) string {
	return "SYNC " + strconv.Itoa(idSender) + "|" + c.Reservation.Client + "|" + c.ToString()
}
