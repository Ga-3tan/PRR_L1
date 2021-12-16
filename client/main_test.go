package main

import (
	"fmt"
	"hotel/client/clientTcp"
	"hotel/config"
	"hotel/utils"
	"math/rand"
	"net"
	"os"
	"strconv"
	"testing"
)

type Test struct {
	query    string
	response string
}

var conn1 net.Conn
var conn2 net.Conn

func TestMain(m *testing.M) {
	setup()
	code := m.Run()
	shutdown()
	os.Exit(code)
}

func setup() {
	conn1 = clientTcp.Connect(rand.Intn(config.NB_SERVERS))
	conn2 = clientTcp.Connect(rand.Intn(config.NB_SERVERS))
	utils.ReadLn(conn1)
	utils.ReadLn(conn2)
	utils.WriteLn(conn1, "TestUser1")
	utils.WriteLn(conn2, "TestUser2")
	utils.ReadLn(conn1)
	utils.ReadLn(conn2)
}

func shutdown() {
	clientTcp.Disconnect(conn1)
	clientTcp.Disconnect(conn2)
}

func TestBookSameRoomTwice(t *testing.T) {
	tests := []Test{
		{"BOOK 1 1 1", "OK votre chambre a été réservée"},
		{"BOOK 1 1 1", "ERR votre chambre est déjà réservée"},
	}
	doTest(tests, t)
}

func TestBookRoomSynchronous(t *testing.T) {
	tests := []Test{
		{"BOOK 7 5 3", "OK votre chambre a été réservée"},
		{"BOOK 8 5 3", "OK votre chambre a été réservée"},
		{"BOOK 9 5 3", "OK votre chambre a été réservée"},
	}
	doSynchronousTest(tests, false, t)
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
		{"BOOM 1 1 1", "ERR Commande inconnue"},
		{"BOOK 1 1", "ERR Arguments manquants ou invalides : BOOK <no chambre> <jour arrivée> <nb nuits>"},
		/* There shouldn't be any extra spaces between parameters */
		{"BOOK 1  1 1 ", "ERR Arguments manquants ou invalides : BOOK <no chambre> <jour arrivée> <nb nuits>"},
		{"BOOK a b c", "ERR Argument invalide"},
		{"ROOMS", "ERR Arguments manquants ou invalides : ROOMS <jour>"},
		{"ROOMS a", "ERR Argument invalide"},
		{"FREE 1", "ERR Arguments manquants ou invalides : FREE <jour arrivée> <nb nuits>"},
		{"FREE 1a 2", "ERR Argument invalide"},
	}
	doTest(tests, t)
}

func TestInputOverflow(t *testing.T) {
	tests := []Test{
		{"BOOK 1 0 1", "ERR jour invalide"},
		{"BOOK 1 " + strconv.Itoa(config.MAX_DAYS+1) + " 1", "ERR jour invalide"},

		{"BOOK 0 1 1", "ERR chambre invalide"},
		{"BOOK " + strconv.Itoa(config.MAX_ROOMS+1) + " 1 1", "ERR chambre invalide"},

		{"BOOK 1 1 0", "ERR nombre de nuits invalide"},
		{"BOOK 1 " + strconv.Itoa(config.MAX_DAYS) + " 10", "ERR nombre de nuits invalide"},
	}
	doTest(tests, t)
}

func TestFreeRoomWhenAllRoomsForTheDayAreFull(t *testing.T) {

	// for each room, we book day 15 and 16
	for i := 1; i <= config.MAX_ROOMS; i++ {
		utils.WriteLn(conn1, "BOOK "+strconv.Itoa(i)+" 15 2")
		utils.ReadLn(conn1)
	}

	tests := []Test{
		{"FREE 15 1", "ERR aucune chambre disponible"},
		{"FREE 16 1", "ERR aucune chambre disponible"},
		{"FREE 17 1", "OK chambre 1 disponible"},
	}

	doTest(tests, t)
	doSynchronousTest(tests, true, t)
}

func doTest(tests []Test, t *testing.T) {
	for index, test := range tests {
		// Sends the query to the server
		fmt.Println("Testing query :")
		fmt.Println(test.query)
		utils.WriteLn(conn1, test.query)

		// Reads the server reply
		reply := utils.ReadLn(conn1)
		fmt.Println("Server response :")
		fmt.Println(reply)

		// Check if the test passes
		if reply != test.response {
			fmt.Println(">> Test id " + strconv.Itoa(index) + " -> FAIL")
			t.Error("\nRECEIVED\n" + reply + "\nEXPECTED\n" + test.response)
		} else {
			fmt.Println(">> Test id " + strconv.Itoa(index) + " -> SUCCESS")
		}
		fmt.Println()
	}
}

func doSynchronousTest(tests []Test, sameAnswer bool, t *testing.T) {
	for index, test := range tests {
		// Sends the query to the server
		fmt.Println("Testing synchronous query :")
		fmt.Println(test.query)
		utils.WriteLn(conn1, test.query)
		utils.WriteLn(conn2, test.query)

		// Reads the server reply
		reply1 := utils.ReadLn(conn1)
		reply2 := utils.ReadLn(conn2)
		fmt.Println("Server response to TestUser1 :")
		fmt.Println(reply1)
		fmt.Println("Server response to TestUser2 :")
		fmt.Println(reply2)

		// Check if the test passes
		if !sameAnswer && reply1 == reply2 {
			fmt.Println(">> Test id " + strconv.Itoa(index) + " -> FAIL")
			t.Error("\nRECEIVED\n" + reply1 + "\nSAME AS\n" + reply2 + "\nCHECK FOR CONCURRENCY PROBLEMS")
		} else if reply1 != test.response && reply2 != test.response {
			fmt.Println(">> Test id " + strconv.Itoa(index) + " -> FAIL")
			t.Error("\nRECEIVED\n" + reply1 + "\n" + reply2 + "\nEXPECTED\n" + test.response)
		} else {
			fmt.Println(">> Test id " + strconv.Itoa(index) + " -> SUCCESS")
		}
		fmt.Println()
	}
}
