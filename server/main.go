package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"net"
	"strings"
	"strconv"
	"hotel/logic"
)

/**
Handles the connexion of a new client
 */
func handleNewSocket(socket net.Conn) {
	input := bufio.NewScanner(socket)

	// Waits for client input and responds
	for input.Scan() {
		// Handles the client command
		result, err := handleClientMessage(input.Text())

		// Displays error message
		if err != nil {
			fmt.Fprintln(socket, err.Error())
		} else {
			// Checks the STOP command
			if result == "STOP" { break }

			// No STOP, returns the result
			fmt.Fprintln(socket, result)
		}
	}

	socket.Close()
}

/**
Handles a received client message
 */
func handleClientMessage(msg string) (string, error) {
	// Splits the command string
	args := strings.Split(msg, " ")

	if len(args) == 0 { return "", errors.New("ERR Aucune commande spécifiée") }

	// Identifies the command
	switch args[0] {
	case "BOOK":
		// Checks number of args
		if len(args) != 4 { return "", errors.New("ERR Arguments manquants BOOK <no chambre> <jour arrivée> <nb nuits>") }

		// Transform args into integers
		nRoom, roomErr := strconv.Atoi(args[1])
		day, dayErr := strconv.Atoi(args[2])
		nbNights, nightErr := strconv.Atoi(args[3])

		if roomErr != nil || dayErr != nil || nightErr != nil {
			return "", errors.New("ERR Argument invalide")
		}

		// All arguments valid, tries to book the room
		ret, err := logic.HOTEL.BookRoom(nRoom, day, nbNights, "Jean")
		return ret, err
	case "ROOMS":
		// All arguments valid, tries to book the room
		ret, err := logic.HOTEL.GetRoomsList()
		return ret, err
	case "FREE":
		return "OK La chambre x est disponible", nil
	case "STOP":
		return "STOP", nil
	default:
		return "ERR Commande inconnue", nil
	}
}

func parseMessage(msg string) {
	strings.Split(msg, " ")
}

/**
Entry point for the server app
 */
func main() {
	// Listens on localhost:8000
	serverSocket, err := net.Listen("tcp", "localhost:8000")

	// Creates the hotel instance

	// Error handle
	if err != nil {
		log.Fatal(err)
	}
	defer serverSocket.Close()

	// Accepts new connexions and handles them
	for {
		newSocket, err := serverSocket.Accept()

		// Error handle
		if err != nil {
			log.Print(err)
			continue
		}

		// Handles the new connexion
		go handleNewSocket(newSocket)
	}
}
