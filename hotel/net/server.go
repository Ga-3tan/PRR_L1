package net

import (
	"bufio"
	"fmt"
	"hotel/cmd"
	"hotel/hotel/config"
	"hotel/hotel/logic"
	"log"
	"net"
	"strconv"
)

func Start(hotel* logic.Hotel) {
	// Start goroutine handling commands
	var commandsChan = make(chan cmd.Command)
	go cmd.CommandHandler(hotel, commandsChan)

	// Listens
	serverSocket, err := net.Listen("tcp", config.HOST + ":" + strconv.Itoa(config.PORT))
	log.Println("LOG Server started on " + config.HOST + ":" + strconv.Itoa(config.PORT))

	// Error handle
	if err != nil {
		log.Fatal(err)
	}
	defer serverSocket.Close()

	// Accepts new connexions and handles them
	log.Println("LOG Now accepting new client connexions")
	for {
		newSocket, err := serverSocket.Accept()

		// Error handle
		if err != nil {
			log.Print(err)
			continue
		}

		// Handles the new connexion
		log.Println("LOG New connexion from " + newSocket.RemoteAddr().String())
		go handleNewClient(newSocket, commandsChan)
	}
}

/**
Handles the connexion of a new client
*/
func handleNewClient(socket net.Conn, commandsChan chan cmd.Command) {
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
				if outputCmd.Cmd == cmd.STOP {
					log.Println("LOG Aborting connexion from " + socket.RemoteAddr().String())
					break
				}

				// Executes the command
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