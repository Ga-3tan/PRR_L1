// Package serverTcp contains all network interactions, requests and clients management
package serverTcp

import (
	"bufio"
	"hotel/config"
	"hotel/server/cmd"
	"hotel/server/logic"
	"hotel/utils"
	"log"
	"net"
	"strconv"
)

// Start runs a new instance of a server
func Start(hotel* logic.Hotel) {
	// Start goroutine handling commands
	var commandsChan = make(chan cmd.Command)
	go cmd.CommandHandler(hotel, commandsChan)

	// Listens
	serverSocket, err := net.Listen("tcp", config.HOST+ ":" + strconv.Itoa(config.PORT))
	log.Println("LOG Server started on " + config.HOST + ":" + strconv.Itoa(config.PORT))

	// Error handle
	if err != nil {
		log.Fatal(err)
	}
	defer func(serverSocket net.Listener) {
		err := serverSocket.Close()
		if err != nil {
			log.Fatal(err)
		}
	}(serverSocket)

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

// handleNewClient is used to manage a new connexion from a client
// It records the client inputs
func handleNewClient(socket net.Conn, commandsChan chan cmd.Command) {
	clientName := ""
	input := bufio.NewScanner(socket)

	// Greets the client
	utils.WriteLn(socket, "Bienvenue dans le système de gestion de réservations de l'server. Veuillez spécifier votre nom : ")

	// Waits for client input and responds
	for input.Scan() {
		userInput := input.Text()

		if clientName == "" {
			if userInput == "" {
				utils.WriteLn(socket, "Veuillez spécifier votre nom : ")
			} else {
				clientName = userInput
				utils.WriteLn(socket, "Bienvenue " + clientName + " ! Utilisez STOP pour quitter.")
			}
		} else {
			// Handles the client command
			outputCmd, err := cmd.ParseCommand(input.Text(), clientName)

			// Displays error message
			if err != nil {
				utils.WriteLn(socket, err.Error())
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
				utils.WriteLn(socket, cmdResult)
			}
		}
	}
	log.Println("LOG Closing client handler " + socket.RemoteAddr().String())

	// Closes the connexion
	err := socket.Close()
	if err != nil {
		log.Println(err)
	}
}