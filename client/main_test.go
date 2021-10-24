package main

import (
	"fmt"
	"hotel/client/clientTcp"
	"hotel/client/utils"
	"net"
	"os"
	"strconv"
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
	conn = clientTcp.Connect()
	utils.ReadLn(conn)
	utils.WriteLn(conn, "TestUser")
	utils.ReadLn(conn)
}

func shutdown() {
	clientTcp.Disconnect(conn)
}

func TestBookSameRoomTwice(t *testing.T) {
	tests := []Test{
		{"BOOK 1 1 1", "OK votre chambre a été réservée"},
		{"BOOK 1 1 1", "ERR votre chambre est déjà réservée"},
	}
	doTest(tests, t)
}

func TestBookSameRoomWithOverlappingStay(t *testing.T) {
	tests := []Test{
		{"BOOK 2 1 5", "OK votre chambre a été réservée"},
		{"BOOK 2 3 5", "ERR votre chambre est déjà réservée"},
		{"BOOK 2 6 2", "OK votre chambre a été réservée"},
	}
	doTest(tests, t)
}

func TestWrongInput(t *testing.T) {
	tests := []Test{
		{"BOOM 1 1 1", "ERR commande inconnue"},
		{"BOOK 1 1", "ERR Arguments manquants ou invalides : BOOK <no chambre> <jour arrivée> <nb nuits>"},
		{"BOOK a b c", "ERR Argument invalide"},
		{"ROOMS", "ERR Arguments manquants ou invalides : ROOMS <jour>"},
		{"ROOMS a", "ERR Argument invalide"},
		{"FREE 1","ERR Arguments manquants ou invalides : ROOMS <jour>"},
		{"FREE a b", ""},
	}
}

func doTest(tests []Test, t *testing.T) {
	for index, test := range tests {
		// Sends the query to the server
		fmt.Println("Testing query :")
		fmt.Println(test.query)
		utils.WriteLn(conn, test.query)

		// Reads the server reply
		reply, _ := utils.ReadLn(conn)
		fmt.Println("Server response :")
		fmt.Println(reply)

		// Check if the test passes
		if reply != test.response {
			fmt.Println(">> Test id " + strconv.Itoa(index) +  " -> FAIL")
			t.Error("\nRECEIVED\n" + reply + "\nEXPECTED\n"+ test.response)
		} else {
			fmt.Println(">> Test id " + strconv.Itoa(index) +  " -> SUCCESS")
		}
		fmt.Println()
	}
}