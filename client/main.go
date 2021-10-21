package main

import (
	"bufio"
	"log"
	"net"
	"os"
	"strconv"
	"strings"
)

type Test struct {
	query string
	response string
}

func writeStr(str string, writer bufio.Writer) {
	_, err := writer.WriteString(str + "\n")
	if err != nil {
		println("Write to server failed:", err.Error())
		os.Exit(1)
	}
	println(str)
}

func readLineUntil(str string, reader bufio.Reader) {
	var reply string
	var err error

	for !strings.Contains(reply, str) {
		reply, err = reader.ReadString('\n')
		if err != nil {
			println("Read to server failed:", err.Error())
			os.Exit(1)
		}
		println(reply)
	}
}

func readLine(reader bufio.Reader) (string, error) {
	return reader.ReadString('\n')
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

	tests := []Test{
		{"BOOK 2 1 1", "OK votre chambre a été réservée"},
		//{"",""},
	}

	reader := *bufio.NewReader(socket)
	writer := *bufio.NewWriter(socket)

	readLineUntil("Veuillez spécifier votre nom :", reader)
	writeStr("UserTest_1", writer)
	readLineUntil(">> ", reader)

	//for !strings.Contains(string(reply), "Veuillez spécifier votre nom :") {
	//	_, err = socket.Read(reply)
	//	if err != nil {
	//		println("Read to server failed:", err.Error())
	//		os.Exit(1)
	//	}
	//	println(string(reply))
	//}

	//for !strings.Contains(string(reply2), ">>") {
	//	_, err = socket.Read(reply2)
	//	if err != nil {
	//		println("Read to server failed:", err.Error())
	//		os.Exit(1)
	//	}
	//	println(string(reply2))
	//}

	for index, test := range tests {
		println(test.query)
		//_, err = socket.Write([]byte(test.query + "\n"))
		//if err != nil {
		//	println("Write to server failed:", err.Error())
		//	os.Exit(1)
		//}
		writeStr(test.query, writer)
		reply, err := readLine(reader)
		println("responses : " + reply)
		//reply3 := make([]byte, 1024)
		//_, err = socket.Read(reply3)
		if err != nil {
			println("Read to server failed:", err.Error())
			os.Exit(1)
		}

		if !strings.Contains(reply , test.response) {
			println("test number " + strconv.Itoa(index) + " failed")
		} else {
			println("test number " + strconv.Itoa(index) + " success")
		}
	}

	// Closes the socket and waits for goroutine to finish
	socket.Close()
}
