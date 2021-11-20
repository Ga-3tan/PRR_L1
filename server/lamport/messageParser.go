package lamport

import (
	"errors"
	"strconv"
	"strings"
)

func ParseMessage(msg string) (MessageLamport, error){
	args := strings.Split(msg, " ")

	if len(args) == 0 {
		return MessageLamport{}, errors.New("ERR Aucune commande spécifiée")
	}

	messageType := MessageType(args[0])
	H, err := strconv.Atoi(args[1])
	if err != nil {
		return MessageLamport{}, errors.New("ERR Message érroné")
	}

	senderId, err := strconv.Atoi(args[2])
	if err != nil {
		return MessageLamport{}, errors.New("ERR Message érroné")
	}

	return MessageLamport{messageType, H, senderId}, nil
}
