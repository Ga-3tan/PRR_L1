package main

import (
	"fmt"
	"log"
	"net"
	"os"
	"strconv"
	"strings"
)

func readLineUntil(str string, conn net.Conn) {
	for !strings.Contains(readLine(conn), str) {}
}

func readLine(conn net.Conn) string {
	reply := make([]byte, 1024)
	_, err := conn.Read(reply)
	if err != nil {
		println("Read to server failed:", err.Error())
		os.Exit(1)
	}
	println(string(reply))
	return string(reply)
}

func writeStr(str string, conn net.Conn) {
	_, err := fmt.Fprint(conn, str)
	if err != nil {
		println("Write to server failed:", err.Error())
		os.Exit(1)
	}
}

func writelnStr(str string, conn net.Conn) {
	writeStr(str + "\n", conn)
}

func Connect() net.Conn {
	conn, err := net.Dial("tcp", "localhost:8000")
	if err != nil {
		log.Fatal(err)
	}
	return conn
}

func Disconnect(conn net.Conn) {
	err := conn.Close()
	if err != nil {
		return 
	}
}

/**
Entry point for the client app
*/
func main() {
	// Connects to the hotel on localhost:8000
	conn, err := net.Dial("tcp", "localhost:8000")
	if err != nil {
		log.Fatal(err)
	}
	defer func(conn net.Conn) {
		err := conn.Close()
		if err != nil {
			log.Fatal(err)
		}
	}(conn)

	tests := []Test{
		{"BOOK 1 1 1", "OK votre chambre a été réservée"},
		{"BOOK 1 1 1", "ERR votre chambre est déjà réservée"},
	}

	readLineUntil("Veuillez spécifier votre nom :", conn)
	writelnStr("UserTest_1", conn)
	readLineUntil(">> ", conn)

	for index, test := range tests {
		println(test.query)
		writelnStr(test.query, conn)
		reply := readLine(conn)
		println("responses : " + reply)
		if !strings.Contains(reply , test.response) {
			println("test number " + strconv.Itoa(index) + " failed")
		} else {
			println("test number " + strconv.Itoa(index) + " success")
		}
	}
}
