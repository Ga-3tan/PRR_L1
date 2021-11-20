// Package serverTcp contains all network interactions, requests and clients management
package serverTcp

import (
	"bufio"
	"hotel/server/cmd"
	"hotel/server/lamport"
	"hotel/server/logic"
	"hotel/server/network"
	"hotel/utils"
	"log"
	"net"
)

// Start runs a new instance of a server
func Start(srvId int, hotel *logic.Hotel) {

	/* channels */
	var serverMutexCh = make(chan lamport.MessageType)         // server <=> mutex
	var mutexConnManagerCh = make(chan lamport.MessageLamport) //  mutex <=> network
	var commandsChan = make(chan cmd.Command)
	var cliCh = make(chan net.Conn)
	var agreedSC = make(chan struct{})

	connManager := network.ConnManager{
		Id:                 srvId,
		Conns:              make(map[int]net.Conn),
		CliCh:              cliCh,
		CmdCh:              commandsChan,
		MutexConnManagerCh: mutexConnManagerCh,
	}

	mutexManager := lamport.MutexManager{
		SelfId:             srvId,
		AgreedSC:           agreedSC,
		ServerMutexCh:      serverMutexCh,
		MutexConnManagerCh: mutexConnManagerCh,
		ConnManager:        connManager,
	}

	// Start goroutine handling commands
	go cmd.CommandHandler(hotel, commandsChan, serverMutexCh,  agreedSC, connManager)

	// Start goroutine mutex process
	go mutexManager.Start()

	// Start goroutine network process
	go connManager.AcceptConnections()

	// Connect to all servers (wait until all connected)
	connManager.ConnectAll() // TODO à voir s'il faut une confirmation de tous les autres serveurs

	// Waits for clients connection
	for newSocket := range cliCh {
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
	utils.WriteLn(socket, "Bienvenue dans le système de gestion de réservations de l'hotel. Veuillez spécifier votre nom : ")

	// Waits for client input and responds
	for input.Scan() {
		userInput := input.Text()

		if clientName == "" {
			if userInput == "" {
				utils.WriteLn(socket, "Veuillez spécifier votre nom : ")
			} else {
				clientName = userInput
				utils.WriteLn(socket, "Bienvenue "+clientName+" ! Utilisez STOP pour quitter.")
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
				cmdResult := <-outputCmd.ReturnContent

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
