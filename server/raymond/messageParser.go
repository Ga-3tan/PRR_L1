package raymond

import (
	"errors"
	"strconv"
	"strings"
)

// ParseMessage Transforms a textual message into a Raymond message object
func ParseMessage(msg string) (MessageRaymond, error) {
	args := strings.Split(strings.ReplaceAll(msg, "RAYM ", ""), " ")

	if len(args) == 0 {
		return MessageRaymond{}, errors.New("ERR Wrong message : bad args")
	}

	messageType := MessageType(args[0])

	senderId, err := strconv.Atoi(args[1])
	if err != nil {
		return MessageRaymond{}, errors.New("ERR Wrong message : bad senderId")
	}

	return MessageRaymond{messageType, senderId}, nil
}
