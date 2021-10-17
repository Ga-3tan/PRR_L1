package main

import (
	"log"
	"net"
	"os"
)

type Test struct {
	query string
	response string
}

/**
Entry point for the client app
*/
func main() {
	// Connects to the hotel on localhost:8000
	socket, err := net.Dial("tcp", "localhost:8000")
	if err != nil {
		log.Fatal(err)
	}
	defer socket.Close()

	tests := {Test{query: "", response:""}}

	_, err = socket.Write([]byte("hello"))
	if err != nil {
		println("Write to server failed:", err.Error())
		os.Exit(1)
	}

	reply := make([]byte, 1024)
	_, err = socket.Read(reply)
	if err != nil {
		println("Read to server failed:", err.Error())
		os.Exit(1)
	}

	// Closes the socket and waits for goroutine to finish
	socket.Close()
}
