// Package clientTcp contains all network interactions for the client side
package clientTcp

import (
	"hotel/config"
	"io"
	"log"
	"net"
	"os"
	"strconv"
)

func Start() {
	// Connects to the server
	conn := Connect()

	// Copies the os input into the created socket
	endOfComm := make(chan struct{})
	go func() {
		io.Copy(os.Stdout, conn)
		endOfComm <- struct{}{}
	}()

	// Copies the received data from the socket to os input
	if _, err := io.Copy(conn, os.Stdin); err != nil {}

	// Closes the socket and waits for goroutine to finish
	Disconnect(conn)
	<-endOfComm
}

func Connect() net.Conn {
	// Connects to the server on localhost:8000
	conn, err := net.Dial("tcp", config.HOST + ":" + strconv.Itoa(config.PORT))
	if err != nil {
		log.Fatal(err)
	}
	return conn
}

func Disconnect(conn net.Conn) {
	err := conn.Close()
	if err != nil {
		log.Fatal(err)
	}
}