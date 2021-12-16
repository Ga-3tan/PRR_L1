// Package network contains all sockets and net connections related structures
package network

import (
	"bufio"
	"hotel/config"
	"hotel/server/cmd"
	"hotel/server/raymond"
	"hotel/utils"
	"log"
	"net"
	"strconv"
	"time"
)

// ConnManager One of the three main components of the program, represents the network bloc
type ConnManager struct {
	Id                 int
	Conns              map[int]net.Conn
	CliCh              chan net.Conn
	CmdSyncCh          chan cmd.SyncCommand
	MutexConnManagerCh chan raymond.MessageRaymond
	WaitSyncCh         chan struct{}
	Sem                chan struct{}
	nbSyncs            int
	nbRdyChildren      int
	canAcceptClients   bool
}

// AcceptConnections Goroutine that listens for incoming connections (servers or clients) on the server port
func (mg *ConnManager) AcceptConnections() {

	// Accept new connections
	port := strconv.Itoa(config.Servers[mg.Id].Port)
	serverSocket, err := net.Listen("tcp", config.Servers[mg.Id].Host+":"+port)
	log.Println("ConnManager>> Server listening on " + config.Servers[mg.Id].Host + ":" + port)

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
	log.Println("ConnManager>> Now accepting new server connexions")
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
			} else {
				utils.WriteLn(newSocket, "Le serveur ne peut accepter de nouvelles connections pour le moment, veuillez réessayer plus tard")
				newSocket.Close()
			}

		default:
			log.Println("ConnManager>> Unrecognised message from " + newSocket.RemoteAddr().String())
			newSocket.Close()
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
				mg.Sem <- struct{}{}

			} else if incomingInput[0:4] == string(STRT) {
				log.Println("ConnManager>> Received STRT, propagating to all children")
				mg.SendAllChildren(string(STRT))
				mg.canAcceptClients = true
				log.Println("SERVER>> Server now ready to accept clients requests")

			} else if incomingInput[0:4] == string(RAYM) { // Lamport command
				msg, err := raymond.ParseMessage(incomingInput)
				if err != nil {
					log.Fatal(err)
				}
				mg.MutexConnManagerCh <- msg

			} else if incomingInput[0:4] == string(SYOK) { // Sync OK
				// Parse SYOK
				msg, err := parseSyokMessage(incomingInput)
				if err != nil {
					log.Fatal(err)
				}

				// Not the SYNC author, propagates to all except sender
				if msg.DestId != mg.Id {
					log.Println("ConnManager>> Received SYOK from " + strconv.Itoa(msg.FromId) + " destined to " + strconv.Itoa(msg.DestId) + ". Forwarding.")
					newSyok := SyokMessage{FromId: mg.Id, DestId: msg.DestId}
					mg.SendAllSiblingsExcept(msg.FromId, newSyok.ToString())
				} else {
					// Original author of the SYNC command
					log.Println("ConnManager>> Received one SYOK for me")
					mg.nbSyncs++
					if mg.nbSyncs == len(config.Servers)-1 { // All pool has sync the command
						log.Println("ConnManager>> Received all" + strconv.Itoa(mg.nbSyncs) + " SYOK, command was synced in all servers")
						mg.nbSyncs = 0
						mg.WaitSyncCh <- struct{}{}
					}
				}

			} else if incomingInput[0:4] == string(SYNC) { // Server Sync command
				outputCmd, err := cmd.ParseServerSyncCommand(incomingInput)
				if err != nil {
					log.Fatal(err)
				}
				mg.CmdSyncCh <- outputCmd

				// Propagates the sync
				mg.SendAllSiblingsExcept(outputCmd.FromId, outputCmd.Command.ToSyncStringCommand(outputCmd.AuthorId, mg.Id))
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
		srvToConnect := config.Servers[mg.Id].Siblings[i]
		log.Println("ConnManager>> Connecting to " + strconv.Itoa(srvToConnect))
		for {
			// Dials the remote server and adds the socket in the map
			conn, err := net.Dial("tcp", config.Servers[srvToConnect].Host+":"+strconv.Itoa(config.Servers[srvToConnect].Port))
			if err == nil {
				utils.WriteLn(conn, string(SRV))
				mg.Conns[srvToConnect] = conn
				break
			}
		}
	}

	// Log
	log.Println("ConnManager>> Connected to all siblings")

	// Waits to send RDY or STRT
	mg.checkAllReady()
}

// checkAllReady handles the RDY messages from the childen
func (mg *ConnManager) checkAllReady() {
	// Gets number of siblings
	nbSiblingsToWait := len(config.Servers[mg.Id].Siblings)
	parent := config.Servers[mg.Id].Parent

	// If not root, doesn't count the parent sibling
	if parent != -1 {
		nbSiblingsToWait -= 1
	}

	// Sends RDY message to parent if all siblings are connected and all children responded with RDY
	for i := 0; i < nbSiblingsToWait; i++ {
		<-mg.Sem
	}

	// Sends RDY to parent or sends STRT if ROOT
	if parent != -1 {
		// Sends RDY to parent
		log.Println("ConnManager>> All children ready, sending ready to parent " + strconv.Itoa(parent))

		// BUGFIX FOR INTERCONNEXION PROBLEM
		log.Println("ConnManager>> Sleeping for " + strconv.Itoa(mg.Id*100) + " ms before sending RDY")
		time.Sleep(time.Duration(mg.Id*100) * time.Millisecond)

		utils.WriteLn(mg.Conns[parent], string(RDY))
	} else {
		log.Println("ConnManager>> ROOT sending start to all children")
		mg.SendAllChildren(string(STRT))

		// Root can start too
		mg.canAcceptClients = true
		log.Println("SERVER>> Server now ready to accept clients requests")
	}
}

// SendAllSiblings Sends a textual message to all siblings of the server
func (mg *ConnManager) SendAllSiblings(msg string) {
	for _, conn := range mg.Conns {
		utils.WriteLn(conn, msg)
	}
}

func (mg *ConnManager) SendAllSiblingsExcept(exceptId int, msg string) {
	for id, conn := range mg.Conns {
		if id != exceptId {
			utils.WriteLn(conn, msg)
		}
	}
}

// SendAllChildren Sends a textual message to all children of the server
func (mg *ConnManager) SendAllChildren(msg string) {
	parent := config.Servers[mg.Id].Parent
	for id, conn := range mg.Conns {
		if parent != id {
			utils.WriteLn(conn, msg)
		}
	}
}

// SendTo Sends a textual payload to a given server
func (mg *ConnManager) SendTo(serverId int, msg string) {
	utils.WriteLn(mg.Conns[serverId], msg)
}
