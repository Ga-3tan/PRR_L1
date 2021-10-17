package main

import (
	"bufio"
	"fmt"
	"hotel/cmd"
	"hotel/logic"
	"log"
	"net"
	"os"
	"strconv"
	"time"
)

/**
Handles the connexion of a new client
*/
func handleNewClient(socket net.Conn, commandsChan chan cmd.Command, debugMode bool) {
	clientName := ""
	input := bufio.NewScanner(socket)

	// Greets the client
	fmt.Fprint(socket, "Bienvenue dans le système de gestion de réservations de l'hotel."+
		"\nVeuillez spécifier votre nom : ")

	// Waits for client input and responds
	for input.Scan() {
		userInput := input.Text()

		if clientName == "" {
			if userInput == "" {
				fmt.Fprint(socket, "Veuillez spécifier votre nom : ")
			} else {
				clientName = userInput
				fmt.Fprint(socket, "Bienvenue " + clientName + " ! Utilisez STOP pour quitter.\n>> ")
			}
		} else {
			// Handles the client command
			outputCmd, err := cmd.ParseCommand(input.Text(), clientName)

			// Displays error message
			if err != nil {
				fmt.Fprintln(socket, err.Error())
			} else {
				// Checks if command is STOP
				if outputCmd.Cmd == cmd.STOP { break }

				// Executes the command
				if debugMode {
					time.Sleep(10 * time.Second)
				}

				commandsChan <- outputCmd
				cmdResult := <- outputCmd.ReturnContent

				// No STOP, returns the result
				fmt.Fprintln(socket, cmdResult)
			}

			fmt.Fprint(socket, ">> ")
		}
	}
	socket.Close()
}

/**
Entry point for the hotel app
*/
func main() {
	// Retrieves program arguments
	debugMode := false
	if len(os.Args) < 3 {
		log.Fatal("Nombre d'arguments invalide : <nbMaxJours> <nbMaxChambres> <modeDebug>")
	} else if len(os.Args) == 4 && os.Args[3] == "debug" {
		debugMode = true
	}

	maxDays, daysErr := strconv.Atoi(os.Args[1])
	maxRooms, roomsErr := strconv.Atoi(os.Args[2])

	// Errors check
	if daysErr != nil || roomsErr != nil {
		log.Fatal("Les arguments donnés ne sont pas valides" + daysErr.Error())
	}

	// Creates the hotel
	var hotel = logic.Hotel{Reservations: make(map[int][]logic.Reservation), MaxDays: maxDays, MaxRooms: maxRooms}

	// Start goroutine handling commands
	var commandsChan = make(chan cmd.Command)
	go cmd.CommandHandler(&hotel, commandsChan)

	// Listens on localhost:8000
	serverSocket, err := net.Listen("tcp", "localhost:8000")

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
		go handleNewClient(newSocket, commandsChan, debugMode)
	}
}
