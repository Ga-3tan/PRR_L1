package main

import (
	"bufio"
	"fmt"
	"log"
	"net"
)

/**
Handles the connexion of a new client
 */
func handleNewSocket(socket net.Conn) {
	input := bufio.NewScanner(socket)
	for input.Scan() { // handleConn <- netcat client
		fmt.Fprintln(socket, handleClientMessage(input.Text()))
	}

	socket.Close()
}

/**
Handles a received client message
 */
func handleClientMessage(msg string) string {
	switch msg {
	case "BOOK":
		return "00> You booked a room !"
	case "LIST":
		return "00> The rooms 0, 1, 2 and 3 are available"
	default:
		return "01> Unknown command"
	}
}

/**
Entry point for the server app
 */
func main() {
	// Listens on localhost:8000
	serverSocket, err := net.Listen("tcp", "localhost:8000")

	// Error handle
	if err != nil {
		log.Fatal(err)
	}
	defer serverSocket.Close()

	// Accepts new connexions and handles them
	for {
		newSocket, err := serverSocket.Accept()

		// Error handle
		if err != nil {
			log.Print(err)
			continue
		}

		// Handles the new connexion
		go handleNewSocket(newSocket)
	}
}
