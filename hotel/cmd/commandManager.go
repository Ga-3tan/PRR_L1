package cmd

import (
	"errors"
	"hotel/logic"
	"strconv"
	"strings"
)

// CommandHandler Handles concurrency and executes the given commands
func CommandHandler(hotel *logic.Hotel, cmdChan chan Command) {
	for newCmd := range cmdChan {
		var res string
		var err error
		switch newCmd.Cmd {
		case BOOK:
			res, err = hotel.BookRoom(newCmd.Reservation.IdRoom, newCmd.Reservation.Day, newCmd.Reservation.NbNights, newCmd.Reservation.Client)
		case ROOMS:
			res, err = hotel.GetRoomsList(newCmd.Reservation.Day, newCmd.Reservation.Client)
		case FREE:
			res, err = hotel.GetFreeRoom(newCmd.Reservation.Day, newCmd.Reservation.NbNights)
		default:
			err = errors.New("ERR Erreur de commande")
		}

		if err != nil {
			newCmd.ReturnContent <- err.Error()
		} else {
			newCmd.ReturnContent <- res
		}
	}
}

// ParseCommand Parses a textual command
func ParseCommand(msg string, clientName string) (Command, error) {
	// Splits the command string
	args := strings.Split(msg, " ")

	if len(args) == 0 {
		return Command{}, errors.New("ERR Aucune commande spécifiée")
	}

	// Identifies the command
	switch args[0] {
	case "BOOK":
		// Checks number of args
		if len(args) != 4 {
			return Command{}, errors.New("ERR Arguments manquants : BOOK <no chambre> <jour arrivée> <nb nuits>")
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
	case "ROOMS":
		// Checks number of args
		if len(args) != 2 {
			return Command{}, errors.New("ERR Arguments manquants : ROOMS <jour>")
		}

		// Transform args into integers
		day, dayErr := strconv.Atoi(args[1])

		if dayErr != nil {
			return Command{}, errors.New("ERR Argument invalide")
		}

		// All arguments valid, displays the rooms status
		return Command{Cmd: ROOMS, Reservation: logic.Reservation{Client: clientName, Day: day}, ReturnContent: make(chan string)}, nil
	case "FREE":
		// Checks number of args
		if len(args) != 3 {
			return Command{}, errors.New("ERR Arguments manquants : FREE <jour arrivée> <nb nuits>")
		}

		// Transform args into integers
		day, dayErr := strconv.Atoi(args[1])
		nbNights, nightErr := strconv.Atoi(args[2])

		if dayErr != nil || nightErr != nil {
			return Command{}, errors.New("ERR Argument invalide")
		}

		// All arguments valid, looks for an available room
		return Command{Cmd: FREE, Reservation: logic.Reservation{Day: day, NbNights: nbNights}, ReturnContent: make(chan string)}, nil
	case "STOP":
		return Command{Cmd: STOP}, nil
	default:
		return Command{}, errors.New("ERR Commande inconnue")
	}
}