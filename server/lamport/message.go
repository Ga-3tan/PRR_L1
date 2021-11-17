package lamport

import "strconv"

type MessageType string

const (
	REQ MessageType = "REQ"
	ACK MessageType = "ACK"
	REL MessageType = "REL"
)

type Message struct {
	Msg    MessageType
	Stamp  int
	Sender int
}

func (m Message) ToString() string {
	return string(m.Msg) + " " + strconv.Itoa(m.Stamp) + " " + strconv.Itoa(m.Sender)
}
