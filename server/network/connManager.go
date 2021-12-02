// Package network contains all sockets and net connections related structures
package network

import (
	"bufio"
	"hotel/config"
	"hotel/server/cmd"
	"hotel/server/lamport"
	"hotel/utils"
	"log"
	"net"
	"strconv"
)

// ConnManager One of the three main components of the program, represents the network bloc
type ConnManager struct {
	Id                 int
	Conns              map[int]net.Conn
	CliCh              chan net.Conn
	CmdSyncCh          chan cmd.SyncCommand
	MutexConnManagerCh chan lamport.MessageLamport
	WaitSyncCh		   chan struct{}
	nbSyncs			   int
	nbRdyChildren	   int
	canAcceptClients   bool
}

// AcceptConnections Goroutine that listens for incoming connections (servers or clients) on the server port
func (mg *ConnManager) AcceptConnections() {

	// Accept new connections
	port := strconv.Itoa(config.Servers[mg.Id].Port)
	serverSocket, err := net.Listen("tcp", config.Servers[mg.Id].Host+":"+port)
	log.Println("ConnManager>> Server started on " + config.Servers[mg.Id].Host + ":" + port)

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
	log.Println("ConnManager>> Now accepting new client/server connexions")
	for {
		newSocket, err := serverSocket.Accept()

		input := bufio.NewScanner(newSocket)
		input.Scan()
		text := input.Text()

		switch text {
		case string(SRV):
			// New connexion from server
			log.Println("ConnManager>> New connection from " + newSocket.RemoteAddr().String())
			go mg.serverReader(newSocket)
		case string(CLI):
			// New connexion from physical client only if server pool is ready
			if mg.canAcceptClients {
				mg.CliCh <- newSocket
			}
		default:
		}

		// Error handle
		if err != nil {
			log.Print(err)
			continue
		}
	}
}

// ServerReader Goroutine handles received data on a given net socket
func (mg *ConnManager) serverReader(socket net.Conn) {
	// Log
	log.Println("ConnManager>> Now listening incoming messages from " + socket.RemoteAddr().String())

	for {
		input := bufio.NewScanner(socket)

		// Waits for client input and responds
		for input.Scan() {
			incomingInput := input.Text()

			if incomingInput[0:3] == string(RDY) {
				log.Println("ConnManager>> Received one RDY")
				mg.nbRdyChildren++
			} else if incomingInput[0:4] == string(STRT) {
				log.Println("ConnManager>> Received STRT, propagating to all children")
				mg.SendAllChildren(string(STRT))
				mg.canAcceptClients = true
			} else if incomingInput[0:4] == string(LPRT) { // Lamport command
				msg, err := lamport.ParseMessage(incomingInput)
				if err != nil {
					log.Fatal(err)
				}
				mg.MutexConnManagerCh<-msg
			} else if incomingInput[0:4] == string(SYNC) { // Sync OK
				mg.nbSyncs++
				if mg.nbSyncs == len(mg.Conns) { // All pool has sync the command
					mg.nbSyncs = 0
					mg.WaitSyncCh <- struct{}{}
				}
			} else { // Server Sync command
				outputCmd, err := cmd.ParseServerSyncCommand(incomingInput)
				if err != nil {
					log.Fatal(err)
				}
				mg.CmdSyncCh <- outputCmd
			}
		}
	}
}

// ConnectAllSiblings Initiates a new connection to all sibling servers
func (mg *ConnManager) ConnectAllSiblings() {
	// Gets number of siblings
	nbSiblings := len(config.Servers[mg.Id].Siblings)

	// Connects to all siblings
	for i := 0; i < nbSiblings; i++ {
		for {
			// Dials the remote server and adds the socket in the map
			srvToConnect := config.Servers[mg.Id].Siblings[i]
			log.Println("ConnManager>> Connecting to " + strconv.Itoa(srvToConnect))
			conn, err := net.Dial("tcp", config.Servers[srvToConnect].Host+":"+strconv.Itoa(config.Servers[srvToConnect].Port))
			if err == nil {
				utils.WriteLn(conn, string(SRV))
				mg.Conns[i] = conn
				break
			}
		}
	}

	// Log
	log.Println("ConnManager>> Connected to all siblings")

	// Waits to send RDY or STRT
	mg.checkAllReady()
}

// ConnectAll Initiates a new connection to all servers in the pool (except self)
func (mg *ConnManager) ConnectAll() {
	for i := 0; i < len(config.Servers); i++ {
		if i != mg.Id {
			for {
				// Dials the remote server and adds the socket in the map
				conn, err := net.Dial("tcp", config.Servers[i].Host+":"+strconv.Itoa(config.Servers[i].Port))
				if err == nil {
					utils.WriteLn(conn, string(SRV))
					mg.Conns[i] = conn
					break
				}
			}
		}
	}

	// Log connection successful
	log.Println("ConnManager>> Connected to all servers pool")
}

// checkAllReady handles the RDY messages from the childen
func (mg *ConnManager) checkAllReady() {
	// Gets number of siblings
	nbSiblings := len(config.Servers[mg.Id].Siblings)
	parent := config.Servers[mg.Id].Parent

	// Sends RDY message to parent if all siblings are connected and all children responded with RDY
	for {
		if mg.nbRdyChildren >= nbSiblings - 1 {
			break
		}
	}

	// If ROOT, sends the STRT message, else sends RDY to parent
	if parent != -1 {
		// Sends RDY to parent
		log.Println("ConnManager>> Sending ready to parent " + strconv.Itoa(parent))
		utils.WriteLn(mg.Conns[parent], string(RDY))
	} else {
		for _, conn  := range mg.Conns {
			log.Println("ConnManager>> ROOT sending start to all children")
			utils.WriteLn(conn, string(STRT))
		}
	}
}

// SendAllChildren Sends a textual message to all children of the server
func (mg *ConnManager) SendAllChildren(msg string) {
	parent := config.Servers[mg.Id].Parent
	for id, conn  := range mg.Conns {
		if parent != id {
			utils.WriteLn(conn, msg)
		}
	}
}

// SendAll Sends a textual payload to all servers pool (except self)
func (mg *ConnManager) SendAll(msg string) {
	for id := 0; id < len(config.Servers); id++ {
		if mg.Id != id {
			mg.SendTo(id, msg)
		}
	}
}

// SendTo Sends a textual payload to a given server
func (mg *ConnManager) SendTo(serverId int, msg string) {
	utils.WriteLn(mg.Conns[serverId], msg)
}
