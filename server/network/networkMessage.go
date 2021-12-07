package network

import (
	"errors"
	"strconv"
	"strings"
)

// MessageType The type of Lamport related messages
type NetworkMessage string

const (
	SRV  NetworkMessage = "SRV"
	CLI  NetworkMessage = "CLI"
	STRT NetworkMessage = "STRT"
	SYNC NetworkMessage = "SYNC"
	SYOK NetworkMessage = "SYOK"
	RAYM NetworkMessage = "RAYM"
	RDY  NetworkMessage = "RDY"
)

// MessageRaymond represents a Lamport message
type SyokMessage struct {
	FromId int
	DestId int
}

// ToString Textualises a Lamport message
func (m *SyokMessage) ToString() string {
	return string(SYOK) + " " + strconv.Itoa(m.FromId) + " " + strconv.Itoa(m.DestId)
}

func parseSyokMessage(msg string) (SyokMessage, error) {
	args := strings.Split(strings.ReplaceAll(msg, string(SYOK)+" ", ""), " ")

	if len(args) == 0 {
		return SyokMessage{}, errors.New("ERR Wrong message : bad args")
	}

	fromId, err1 := strconv.Atoi(args[0])
	destId, err2 := strconv.Atoi(args[1])
	if err1 != nil || err2 != nil {
		return SyokMessage{}, errors.New("ERR Wrong message : bad args")
	}

	return SyokMessage{FromId: fromId, DestId: destId}, nil
}
