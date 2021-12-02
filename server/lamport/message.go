// Package lamport contains all Lamport algorithm logic
package lamport

import (
	"strconv"
)

// MessageType The type of Lamport related messages
type MessageType string

const (
	REQ MessageType = "REQ"
	ACK MessageType = "ACK"
	REL MessageType = "REL"
	ASK_SC MessageType = "ASK_SC"
	END_SC MessageType = "END_SC"
)

// MessageLamport represents a Lamport message
type MessageLamport struct {
	Type     MessageType
	H        int
	SenderID int
}

// ToString Textualises a Lamport message
func (m *MessageLamport) ToString() string {
	return "LPRT " + string(m.Type) + " " + strconv.Itoa(m.H) + " " + strconv.Itoa(m.SenderID)
}
