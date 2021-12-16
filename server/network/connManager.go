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
	"sync"
	//"time"
)

// ConnManager One of the three main components of the program, represents the network bloc
type ConnManager struct {
	Id                 int
	Conns              map[int]net.Conn
	CliCh              chan net.Conn
	CmdSyncCh          chan cmd.SyncCommand
	MutexConnManagerCh chan raymond.MessageRaymond
	WaitSyncCh         chan struct{}
	mutex              sync.Mutex
	nbSyncs            int
	nbRdyChildren      int
	canAcceptClients   bool
	ready              int
	WaitForAllRdy      chan struct{}
	RdyCh              chan struct{}
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

func (mg *ConnManager) rdyCounter() {
	nbChildrenToWait := len(config.Servers[mg.Id].Siblings)
	parent := config.Servers[mg.Id].Parent

	// If not root, doesn't count the parent sibling
	if parent != -1 {
		nbChildrenToWait -= 1
	}

	log.Println("rdyCounter>> waiting for " + strconv.Itoa(nbChildrenToWait))

	if !mg.checkRdy(nbChildrenToWait) { // if nbChildrenToWait > 0
		for range mg.RdyCh {
			log.Println("rdyCounter>> rdy arrived")
			mg.nbRdyChildren += 1
			if mg.checkRdy(nbChildrenToWait) { // if all ready
				break
			}
		}
	}
	log.Println("rdyCounter>> ending")
}

// checkRdy return true if all RDY
func (mg *ConnManager) checkRdy(nbChildrenToWait int) bool {
	if mg.nbRdyChildren == nbChildrenToWait {
		log.Println("checkRdy>> ALL rdy arrived trying to unlock WaitForAllRdy")
		mg.WaitForAllRdy <- struct{}{}
		log.Println("checkRdy>> WaitForAllRdy unlocked")
		return true
	}
	log.Println("checkRdy>> failed, needs " + strconv.Itoa(nbChildrenToWait-mg.nbRdyChildren) + " more")
	return false
}

// ServerReader Goroutine handles received data on a given net socket
func (mg *ConnManager) serverReader(socket net.Conn) {
	// Log
	log.Println("ConnManager>> Now listening incoming messages from " + socket.RemoteAddr().String())

	mg.ready += 1
	for {
		input := bufio.NewScanner(socket)

		// Waits for client input and responds
		for input.Scan() {
			incomingInput := input.Text()

			if incomingInput[0:3] == string(RDY) {
				log.Println("ConnManager>> Received one RDY : " + incomingInput)
				mg.RdyCh <- struct{}{}

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

				// Not the receiver, propagates to all except sender
				if msg.DestId != mg.Id {
					log.Println("ConnManager>> Received SYOK destined to " + strconv.Itoa(msg.DestId) + ". Forwarding.")
					newSyok := SyokMessage{FromId: mg.Id, DestId: msg.DestId}
					mg.SendAllSiblingsExcept(msg.FromId, newSyok.ToString())
				} else {
					// Original reciever of the SYOK command
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
				mg.SendAllSiblingsExcept(outputCmd.AuthorId, outputCmd.Command.ToSyncStringCommand(mg.Id))
			}
		}
	}
}

// ConnectAllSiblings Initiates a new connection to all sibling servers
func (mg *ConnManager) ConnectAllSiblings() {
	// Gets number of siblings
	nbSiblings := len(config.Servers[mg.Id].Siblings)
	go mg.rdyCounter()

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
				log.Println("ConnManager>> Connected Dial to " + strconv.Itoa(config.Servers[srvToConnect].Port))
				break
			}
		}
	}

	// Log
	log.Println("ConnManager>> Connected to all siblings")

	// Waits for all siblings to send RDY or STRT
	for range mg.WaitForAllRdy {
		break
	}
	log.Println("ConnManager>> RDY from all children")
	mg.sendRdyOrStrt()
}

func (mg *ConnManager) sendRdyOrStrt() {
	parent := config.Servers[mg.Id].Parent
	if parent != -1 {
		// Sends RDY to parent
		log.Println("ConnManager>> All children ready, sending ready to parent " + strconv.Itoa(parent))
		// BUGFIX FOR INTERCONNEXION PROBLEM
		//log.Println("ConnManagerConnManagerConnManagerConnManagerConnManager>> Sleeping for " + strconv.Itoa(mg.Id*100) + " ms before sending RDY")
		//time.Sleep(time.Duration(mg.Id*100) * time.Millisecond)
		utils.WriteLn(mg.Conns[parent], string(RDY)+strconv.Itoa(mg.Id))
	} else {
		log.Println("ConnManager>> ROOT sending start to all children")
		mg.SendAllChildren(string(STRT))

		// Root can start too
		mg.canAcceptClients = true
		log.Println("SERVER>> Server now ready to accept clients requests")
	}
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

	log.Println("should receive" + strconv.Itoa(nbSiblingsToWait))
	if nbSiblingsToWait != 0 {
		for range mg.RdyCh {
			log.Println("received RDY")
			mg.nbRdyChildren++
			log.Println(strconv.Itoa(mg.nbRdyChildren))

			// Sends RDY message to parent if all siblings are connected and all children responded with RDY
			if mg.nbRdyChildren >= nbSiblingsToWait {
				break
			}
		}
	}

	// Sends RDY to parent or sends STRT if ROOT
	if parent != -1 {
		// Sends RDY to parent
		log.Println("ConnManager>> All children ready, sending ready to parent " + strconv.Itoa(parent))
		utils.WriteLn(mg.Conns[parent], string(RDY)+strconv.Itoa(mg.Id))
	} else {
		log.Println("ConnManager>> ROOT sending start to all children")
		mg.SendAllChildren(string(STRT))

		// Root can start too
		mg.canAcceptClients = true
		log.Println("SERVER>> Server now ready to accept clients requests")
	}
	mg.WaitForAllRdy <- struct{}{}
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
	//for mg.ready != len(config.Servers[mg.Id].Siblings) {
	//
	//}
	parent := config.Servers[mg.Id].Parent
	for id, conn := range mg.Conns {
		if parent != id {
			utils.WriteLn(conn, msg)
		}
	}
}

func (mg *ConnManager) SendToParent(msg string) {
	parent := config.Servers[mg.Id].Parent
	utils.WriteLn(mg.Conns[parent], msg)
}

// SendTo Sends a textual payload to a given server
func (mg *ConnManager) SendTo(serverId int, msg string) {
	utils.WriteLn(mg.Conns[serverId], msg)
}
