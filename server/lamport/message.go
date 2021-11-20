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
	ASK_SC MessageType = "ASK_SC"
	END_SC MessageType = "END_SC"
)

type MessageLamport struct {
	Type     MessageType
	H        int
	SenderID int
}

type MessageSync struct {
	Command    cmd.Command
	ClientName string
}

func (m MessageLamport) ToString() string {
	return "LPRT " + string(m.Type) + " " + strconv.Itoa(m.H) + " " + strconv.Itoa(m.SenderID)
}

func (m MessageSync) ToString() string {
	return "SYNC " + m.ClientName + "|" + m.Command.ToString()
}
