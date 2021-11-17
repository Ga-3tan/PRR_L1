package network

import (
	"bufio"
	"hotel/config"
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
				println("New connection from " + newSocket.RemoteAddr().String())
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
}

func (mg *ConnManager) serverReader(socket net.Conn) {
	// Log
	println("Now listening incoming messages from " + socket.RemoteAddr().String())

	for {
		input := bufio.NewScanner(socket)

		// Waits for client input and responds
		for input.Scan() {
			// TODO Read message from socket and send to MUTEX or MessageHandler
		}
	}
}

func (mg *ConnManager) serverWriter(socket net.Conn) {
	// TODO block on write channel and send message to all
}

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

func (mg *ConnManager) SendAll(message lamport.Message) {
	for id, _ := range mg.Conns {
		if mg.Id != id {
			mg.sendTo(id, message)
		}
	}
}

func (mg *ConnManager) sendTo(serverId int, message lamport.Message) {
	utils.WriteLn(mg.Conns[serverId], message.ToString())
}