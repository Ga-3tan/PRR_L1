// Package serverTcp contains all network interactions, requests and clients management
package serverTcp

import (
	"bufio"
	"hotel/config"
	"hotel/server/cmd"
	cmdManager "hotel/server/cmd/manager"
	"hotel/server/logic"
	"hotel/server/network"
	"hotel/server/raymond"
	raymManager "hotel/server/raymond/manager"
	"hotel/utils"
	"log"
	"net"
)

// Start runs a new instance of a server
func Start(srvId int, hotel *logic.Hotel) {

	// Channels initialisation
	var serverMutexCh = make(chan raymond.MessageType, 100)         // server <=> mutex
	var mutexConnManagerCh = make(chan raymond.MessageRaymond, 100) // mutex <=> network
	var commandsChan = make(chan cmd.Command, 100)
	var commandsSyncChan = make(chan cmd.SyncCommand, 100)
	var cliCh = make(chan net.Conn, 100)
	var agreedSC = make(chan struct{})
	var waitSyncCh = make(chan struct{})

	// Network manager (receives and sends network messages in the server pool)
	connManager := network.ConnManager{
		Id:                 srvId,
		Conns:              make(map[int]net.Conn),
		CliCh:              cliCh,
		CmdSyncCh:          commandsSyncChan,
		MutexConnManagerCh: mutexConnManagerCh,
		WaitSyncCh:         waitSyncCh,
		Sem:                make(chan struct{}),
	}

	// Mutex manager, implements Lamport algorithm for distributed operations on the hotel
	mutexManager := raymManager.MutexManager{
		SelfId:             srvId,
		State:              raymManager.N,
		Parent:             config.Servers[srvId].Parent,
		Queue:              make([]int, 0),
		AgreedSC:           agreedSC,
		ServerMutexCh:      serverMutexCh,
		MutexConnManagerCh: mutexConnManagerCh,
		ConnManager:        &connManager,
	}

	// Handles commands to execute on the hotel, uses the mutex to check concurrent accesses and obtain SC access
	commandHandler := cmdManager.CommandManager{
		Hotel:         hotel,
		CmdChan:       commandsChan,
		CmdSyncChan:   commandsSyncChan,
		ServerMutexCh: serverMutexCh,
		AgreedSC:      agreedSC,
		ConnManager:   &connManager,
	}

	// Connects all the servers together
	go connManager.AcceptConnections()
	connManager.ConnectAllSiblings()

	// Start goroutine mutex process
	go mutexManager.Start()

	// Start goroutine handling commands
	go commandHandler.HandleCommands()
	go commandHandler.HandleSyncCommands()

	// Waits for clients connection
	for newSocket := range cliCh {
		// Handles the new connexion
		log.Println("SERVER>> New connexion from client " + newSocket.RemoteAddr().String())
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
