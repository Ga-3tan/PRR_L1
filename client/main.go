package main

import (
	"io"
	"log"
	"net"
	"os"
)

/**
Entry point for the client app
 */
func main() {
	// Connects to the server on localhost:8000
	socket, err := net.Dial("tcp", "localhost:8000")

	// Error handle
	if err != nil {
		log.Fatal(err)
	}
	defer socket.Close()

	// Copies the os input into the created socket
	endOfComm := make(chan struct{})
	go func() {
		io.Copy(os.Stdout, socket)
		log.Println("done")
		endOfComm <- struct{}{}
	}()

	// Copies the received data from the socket to os input
	if _, err := io.Copy(socket, os.Stdin); err != nil {
		log.Fatal(err)
	}

	// Closes the socket and waits for goroutine to finish
	socket.Close()
	<-endOfComm
}