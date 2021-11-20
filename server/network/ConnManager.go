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
	Id    int
	Conns map[int]net.Conn
	CliCh chan net.Conn
	CmdCh chan cmd.Command
	MutexCh chan lamport.MessageLamport
	WriteCh map[int]chan string
}

func (mg *ConnManager) AcceptConnections() {
	for {
		// Accept new connections
		port := strconv.Itoa(config.Servers[mg.Id].Port)
		serverSocket, err := net.Listen("tcp", config.Servers[mg.Id].Host + ":" + port)
		log.Println("LOG Server started on " + config.Servers[mg.Id].Host + ":" + port)

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
		log.Println("LOG Now accepting new client/server connexions")
		for {
			newSocket, err := serverSocket.Accept()

			input := bufio.NewScanner(newSocket)
			input.Scan()
			text := input.Text()

			switch text {
			case "SRV":
				// New connexion from server
				println("New connection from server " + newSocket.RemoteAddr().String())
				go mg.serverReader(newSocket)
			case "CLI":
				// New connexion from physical client
				println("New connection from client " + newSocket.RemoteAddr().String())
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
}

func (mg *ConnManager) serverReader(socket net.Conn) {
	// Log
	println("Now listening incoming messages from " + socket.RemoteAddr().String())

	for {
		input := bufio.NewScanner(socket)

		// Waits for client input and responds
		for input.Scan() {
			// TODO Read message from socket and send to MUTEX
			incomingInput := input.Text()

			println("INCOMING FROM " + socket.RemoteAddr().String() + " : \"" + incomingInput + "\"")

			if incomingInput[0:3] == "LPT" { // Lamport command
				// TODO lamport command : send to mutex
			} else { // Server command
				outputCmd, err := cmd.ParseServerSyncCommand(incomingInput)
				println("SERVER COMMAND : " + outputCmd.ToString())
				if err != nil {
					log.Println(err)
				}
				mg.CmdCh <- outputCmd
			}
		}
	}
}

// TODO serverWriter necessary ?

//// serverWriter Write to one server, block on channel of this server
//func (mg *ConnManager) serverWriter(id int) {
//	for msg := range mg.WriteCh[id] {
//		utils.WriteLn(mg.Conns[id], msg)
//	}
//}
//
//// writeTo to write a msg to one server by id
//func (mg* ConnManager) writeTo(id int, msg string) {
//	mg.WriteCh[id]<-msg
//}
//
//
//// writeToAll to write a msg to all server (except self)
//func (mg* ConnManager) writeToALl(msg string) {
//	for key, _ := range mg.Conns {
//		mg.writeTo(key, msg)
//	}
//}

func (mg *ConnManager) ConnectAll() {
	for i := 0; i < len(config.Servers); i++ {
		if i != mg.Id {
			for {
				// Dials the remote server and adds the socket in the map
				conn, err := net.Dial("tcp", config.Servers[i].Host + ":" + strconv.Itoa(config.Servers[i].Port))
				if err == nil {
					utils.WriteLn(conn, "SRV")
					mg.Conns[config.Servers[i].Port] = conn
					break
				}
			}
		}
	}

	// Log (connexion successful
	println("Connected to all servers pool")
}

func (mg *ConnManager) SendAll(msg string) {
	for id, _ := range mg.Conns {
		if mg.Id != id { // TODO mg.Conns does really contain itself ?
			mg.SendTo(id, msg)
		}
	}
}

func (mg *ConnManager) SendTo(serverId int, msg string) {
	utils.WriteLn(mg.Conns[serverId], msg)
}