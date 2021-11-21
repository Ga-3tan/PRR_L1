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

type ConnManager struct {
	Id                 int
	Conns              map[int]net.Conn
	CliCh              chan net.Conn
	CmdCh              chan cmd.Command
	MutexConnManagerCh chan lamport.MessageLamport
}

func (mg *ConnManager) AcceptConnections() {

	// Accept new connections
	port := strconv.Itoa(config.Servers[mg.Id].Port)
	serverSocket, err := net.Listen("tcp", config.Servers[mg.Id].Host+":"+port)
	log.Println("Server started on " + config.Servers[mg.Id].Host + ":" + port)

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
	log.Println("Now accepting new client/server connexions")
	for {
		newSocket, err := serverSocket.Accept()

		input := bufio.NewScanner(newSocket)
		input.Scan()
		text := input.Text()

		switch text {
		case "SRV":
			// New connexion from server
			log.Println("New connection from " + newSocket.RemoteAddr().String())
			go mg.serverReader(newSocket)
		case "CLI":
			// New connexion from physical client
			mg.CliCh <- newSocket
		default:
		}

		// Error handle
		if err != nil {
			log.Print(err)
			continue
		}
	}
}

func (mg *ConnManager) serverReader(socket net.Conn) {
	// Log
	println("Now listening incoming messages from " + socket.RemoteAddr().String())

	for {
		input := bufio.NewScanner(socket)

		// Waits for client input and responds
		for input.Scan() {
			incomingInput := input.Text()

			if incomingInput[0:4] == "LPRT" { // Lamport command
				msg, err := lamport.ParseMessage(incomingInput)
				if err != nil {
					log.Fatal(err)
				}
				mg.MutexConnManagerCh<-msg
			} else { // Server Sync command
				outputCmd, err := cmd.ParseServerSyncCommand(incomingInput)
				if err != nil {
					log.Fatal(err)
				}
				mg.CmdCh <- outputCmd
			}
		}
	}
}

func (mg *ConnManager) ConnectAll() {
	for i := 0; i < len(config.Servers); i++ {
		if i != mg.Id {
			for {
				// Dials the remote server and adds the socket in the map
				conn, err := net.Dial("tcp", config.Servers[i].Host+":"+strconv.Itoa(config.Servers[i].Port))
				if err == nil {
					utils.WriteLn(conn, "SRV")
					mg.Conns[config.Servers[i].Port] = conn
					break
				}
			}
		}
	}

	// Log (connexion successful
	log.Println("Connected to all servers pool")
}

func (mg *ConnManager) SendAll(msg string) {
	for id := 0; id < len(config.Servers); id++ {
		if mg.Id != id {
			mg.SendTo(id, msg)
		}
	}
}

func (mg *ConnManager) SendTo(serverId int, msg string) {
	utils.WriteLn(mg.Conns[config.Servers[serverId].Port], msg)
}
