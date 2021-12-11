package cmd

import (
	"errors"
	"hotel/server/logic"
	"log"
	"strconv"
	"strings"
)

// ParseCommand Parses a textual command into a Command type
func ParseCommand(msg string, clientName string) (Command, error) {
	// Splits the command string
	args := strings.Split(msg, " ")

	if len(args) == 0 {
		return Command{}, errors.New("ERR Aucune commande spécifiée")
	}

	// Identifies the command
	switch CommandType(args[0]) {
	case BOOK:
		// Checks number of args
		if len(args) != 4 {
			return Command{}, errors.New("ERR Arguments manquants ou invalides : BOOK <no chambre> <jour arrivée> <nb nuits>")
		}

		// Transform args into integers
		nRoom, roomErr := strconv.Atoi(args[1])
		day, dayErr := strconv.Atoi(args[2])
		nbNights, nightErr := strconv.Atoi(args[3])

		if roomErr != nil || dayErr != nil || nightErr != nil {
			return Command{}, errors.New("ERR Argument invalide")
		}

		// All arguments valid, returns the command
		return Command{Cmd: BOOK, Reservation: logic.Reservation{IdRoom: nRoom, Client: clientName, Day: day, NbNights: nbNights}, ReturnContent: make(chan string)}, nil
	case ROOMS:
		// Checks number of args
		if len(args) != 2 {
			return Command{}, errors.New("ERR Arguments manquants ou invalides : ROOMS <jour>")
		}

		// Transform args into integers
		day, dayErr := strconv.Atoi(args[1])

		if dayErr != nil {
			return Command{}, errors.New("ERR Argument invalide")
		}

		// All arguments valid, displays the rooms status
		return Command{Cmd: ROOMS, Reservation: logic.Reservation{Client: clientName, Day: day}, ReturnContent: make(chan string)}, nil
	case FREE:
		// Checks number of args
		if len(args) != 3 {
			return Command{}, errors.New("ERR Arguments manquants ou invalides : FREE <jour arrivée> <nb nuits>")
		}

		// Transform args into integers
		day, dayErr := strconv.Atoi(args[1])
		nbNights, nightErr := strconv.Atoi(args[2])

		if dayErr != nil || nightErr != nil {
			return Command{}, errors.New("ERR Argument invalide")
		}

		// All arguments valid, looks for an available room
		return Command{Cmd: FREE, Reservation: logic.Reservation{Day: day, NbNights: nbNights}, ReturnContent: make(chan string)}, nil
	case STOP:
		return Command{Cmd: STOP}, nil
	default:
		return Command{}, errors.New("ERR Commande inconnue")
	}
}

// ParseServerSyncCommand take a SYNC USER|[commande] et la commande
func ParseServerSyncCommand(msg string) (SyncCommand, error) {
	servSeparator := strings.Split(msg, "|")
	authorId, errConv1 := strconv.Atoi(strings.ReplaceAll(servSeparator[0], "SYNC ", "")) // supprimer "SYNC "
	fromId, errConv2 := strconv.Atoi(servSeparator[1])                                    // supprimer "SYNC "
	if errConv1 != nil || errConv2 != nil {
		log.Fatal("Impossible to convert received data to sync command")
	}
	username := servSeparator[2]
	command, err := ParseCommand(servSeparator[3], username)
	if err != nil {
		log.Println(err)
	}
	return SyncCommand{AuthorId: authorId, FromId: fromId, Command: command}, nil
}
