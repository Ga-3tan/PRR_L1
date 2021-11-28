package lamport

import (
	"errors"
	"strconv"
	"strings"
)

// ParseMessage Transforms a textual message into a Lamport message object
func ParseMessage(msg string) (MessageLamport, error){
	args := strings.Split(strings.ReplaceAll(msg, "LPRT ", ""), " ")

	if len(args) == 0 {
		return MessageLamport{}, errors.New("ERR Wrong message : bad args")
	}

	messageType := MessageType(args[0])

	H, err := strconv.Atoi(args[1])
	if err != nil {
		return MessageLamport{}, errors.New("ERR Wrong message : bad H")
	}

	senderId, err := strconv.Atoi(args[2])
	if err != nil {
		return MessageLamport{}, errors.New("ERR Wrong message : bad senderId")
	}

	return MessageLamport{messageType, H, senderId}, nil
}
