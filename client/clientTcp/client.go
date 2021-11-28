// Package clientTcp contains all network interactions for the client side
package clientTcp

import (
	"hotel/config"
	"hotel/utils"
	"io"
	"log"
	"net"
	"os"
	"strconv"
)

// Start Launches a new client process
func Start(srvNo int) {
	// Connects to the server
	conn := Connect(srvNo)

	// Copies the os input into the created socket
	endOfComm := make(chan struct{})
	go func() {
		io.Copy(os.Stdout, conn)
		endOfComm <- struct{}{}
	}()

	// Copies the received data from the socket to os input
	if _, err := io.Copy(conn, os.Stdin); err != nil {
	}

	// Closes the socket and waits for goroutine to finish
	Disconnect(conn)
	<-endOfComm
}

// Connect initiates a new connection to a given server
func Connect(srvNo int) net.Conn {
	// Connects to the server on localhost:8000
	conn, err := net.Dial("tcp", config.Servers[srvNo].Host+":"+strconv.Itoa(config.Servers[srvNo].Port))
	if err != nil {
		log.Fatal(err)
	}

	// Tells that it's a client connecting
	utils.WriteLn(conn, "CLI")
	return conn
}

// Disconnect stops the connection from the server
func Disconnect(conn net.Conn) {
	err := conn.Close()
	if err != nil {
		log.Fatal(err)
	}
}
