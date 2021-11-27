// Package serverTcp contains all network interactions, requests and clients management
package serverTcp

import (
	"bufio"
	"hotel/server/cmd"
	cmdManager "hotel/server/cmd/manager"
	"hotel/server/lamport"
	lprtManager "hotel/server/lamport/manager"
	"hotel/server/logic"
	"hotel/server/network"
	"hotel/utils"
	"log"
	"net"
)

// Start runs a new instance of a server
func Start(srvId int, hotel *logic.Hotel) {

	/* channels */
	var serverMutexCh = make(chan lamport.MessageType, 100)         // server <=> mutex
	var mutexConnManagerCh = make(chan lamport.MessageLamport, 100) //  mutex <=> network
	var commandsChan = make(chan cmd.Command)
	var cliCh = make(chan net.Conn)
	var agreedSC = make(chan struct{})

	// Network manager (receives and sends network messages in the server pool)
	connManager := network.ConnManager{
		Id:                 srvId,
		Conns:              make(map[int]net.Conn),
		CliCh:              cliCh,
		CmdCh:              commandsChan,
		MutexConnManagerCh: mutexConnManagerCh,
	}

	// Mutex manager, implements Lamport algorithm for distributed operations on the hotel
	mutexManager := lprtManager.MutexManager{
		SelfId:             srvId,
		AgreedSC:           agreedSC,
		ServerMutexCh:      serverMutexCh,
		MutexConnManagerCh: mutexConnManagerCh,
		ConnManager:        connManager,
	}

	// Handles commands to execute on the hotel, uses the mutex to check concurrent accesses and obtain SC access
	commandHandler := cmdManager.CommandManager{
		Hotel: hotel,
		CmdChan: commandsChan,
		ServerMutexCh: serverMutexCh,
		AgreedSC: agreedSC,
		ConnManager: connManager,
	}

	// Connects all the servers together
	go connManager.AcceptConnections()
	connManager.ConnectAll()
	log.Println("SERVER>> Server now ready to accept clients requests")

	// Start goroutine mutex process
	go mutexManager.Start()

	// Start goroutine handling commands
	go commandHandler.HandleCommands()

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
					log.Println("SERVER>> LOG Aborting connexion from " + socket.RemoteAddr().String())
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
	log.Println("SERVER>> LOG Closing client handler " + socket.RemoteAddr().String())

	// Closes the connexion
	err := socket.Close()
	if err != nil {
		log.Println(err)
	}
}
