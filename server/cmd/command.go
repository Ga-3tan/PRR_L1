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

// SyncCommand Synch commands made on another server, contains the id of the remote server and the command to sync
type SyncCommand struct {
	AuthorId int
	Command Command
}

// ToString Textualises a command
func (c *Command) ToString() string {
	return string(c.Cmd) + " " + c.Reservation.ToString()
}

// ToSyncStringCommand Textualises a sync command
func (c *Command) ToSyncStringCommand(idSender int) string {
	return "SYNC " + strconv.Itoa(idSender) + "|" + c.Reservation.Client + "|" + c.ToString()
}
