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
	Type     MessageType
	H        int
	SenderID int
}

type MessageSync struct {
	Command    cmd.Command
	CLientName string
}

func (m MessageLamport) ToString() string {
	return "LPRT " + string(m.Type) + " " + strconv.Itoa(m.H) + " " + strconv.Itoa(m.SenderID)
}

func (m MessageSync) ToString() string {
	return "SYNC " + m.CLientName + "|" + m.Command.ToString()
}
