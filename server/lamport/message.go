package lamport

import (
	"hotel/server/cmd"
	"strconv"
)

type MessageType string

const (
	REQ MessageType = "REQ"
	ACK MessageType = "ACK"
	REL MessageType = "REL"
)

type MessageLamport struct {
	Msg    MessageType
	Stamp  int
	Sender int
}

type MessageSync struct {
	Command cmd.Command
	Sender string
}

func (m MessageLamport) ToString() string {
	return "LPRT " + string(m.Msg) + " " + strconv.Itoa(m.Stamp) + " " + strconv.Itoa(m.Sender)
}

func (m MessageSync) ToString() string {
	return "SYNC " + m.Sender + "|" + m.Command.ToString()
}
