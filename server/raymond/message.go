package raymond

import "strconv"

// MessageType The type of Lamport related messages
type MessageType string

const (
	REQ MessageType = "REQ"
	TOK MessageType = "TOK"
	ASK_SC MessageType = "ASK_SC" // TODO à voir si on peut pas factoriser ça par rapport à message de lamport
	END_SC MessageType = "END_SC"
)

// MessageRaymond represents a Lamport message
type MessageRaymond struct {
	Type     MessageType
	SenderID int
}

// ToString Textualises a Lamport message
func (m *MessageRaymond) ToString() string {
	return "RAYM " + string(m.Type) + " " + strconv.Itoa(m.SenderID)
}