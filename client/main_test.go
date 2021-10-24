package main

import (
	"net"
	"os"
	"strconv"
	"strings"
	"testing"
)

type Test struct {
	query string
	response string
}

var conn net.Conn

func TestMain(m *testing.M) {
	setup()
	code := m.Run()
	shutdown()
	os.Exit(code)
}

func setup() {
	conn = Connect()
	readLineUntil("Veuillez spécifier votre nom :", conn)
	writelnStr("UserTest", conn)
}

func shutdown() {
	Disconnect(conn)
}

func TestBookRoomTwice(t *testing.T) {
	tests := []Test{
		{"BOOK 1 1 1", "OK votre chambre a été réservée"},
		{"BOOK 1 1 1", "ERR votre chambre est déjà réservée"},
	}

	for index, test := range tests {
		readLineUntil(">> ", conn)
		println(test.query)
		writelnStr(test.query, conn)
		reply := readLine(conn)
		println("responses : " + reply)
		if !strings.Contains(reply , test.response) {
			t.Error("BOOK error : " + reply + " should be " + test.response)
		} else {
			println("BOOK " + strconv.Itoa(index) + " SUCCESS")
		}
	}
}