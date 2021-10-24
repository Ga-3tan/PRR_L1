package main

import (
	"fmt"
	"hotel/client/clientTcp"
	"hotel/client/utils"
	"hotel/config"
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
		{"BOOM 1 1 1", "ERR Commande inconnue"},
		{"BOOK 1 1", "ERR Arguments manquants ou invalides : BOOK <no chambre> <jour arrivée> <nb nuits>"},
		/* There shouldn't be any extra spaces between parameters */
		{"BOOK 1  1 1 ", "ERR Arguments manquants ou invalides : BOOK <no chambre> <jour arrivée> <nb nuits>"},
		{"BOOK a b c", "ERR Argument invalide"},
		{"ROOMS", "ERR Arguments manquants ou invalides : ROOMS <jour>"},
		{"ROOMS a", "ERR Argument invalide"},
		{"FREE 1","ERR Arguments manquants ou invalides : FREE <jour arrivée> <nb nuits>"},
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
		utils.WriteLn(conn, "BOOK " + strconv.Itoa(i) + " 15 2")
		utils.ReadLn(conn)
	}

	tests := []Test{
		{"FREE 15 1","ERR aucune chambre disponible"},
		{"FREE 16 1","ERR aucune chambre disponible"},
		{"FREE 17 1","OK chambre 1 disponible"},
	}

	doTest(tests, t)
}

//func TestShowRoomAvailability(t *testing.T) {
//
//	// we book day 12 for room 1-8
//	for i := 1; i <= 8; i++ {
//		utils.WriteLn(conn, "BOOK " + strconv.Itoa(i) + " 12 1")
//		utils.ReadLn(conn)
//	}
//
//	query := "ROOMS 12"
//	fmt.Println("Testing query :")
//	fmt.Println(query)
//	utils.WriteLn(conn, query)
//
//	availability := "RESERVE"
//
//	for i := 1; i <= config.MAX_ROOMS; i++ {
//		reply, _ := utils.ReadLn(conn)
//		fmt.Println("Server response :")
//		fmt.Println(reply)
//		if reply != "| Chambre: " + strconv.Itoa(i) + ", Status: " + availability {
//			fmt.Println(">> Test ROOMS -> FAIL")
//			t.Error("\nRECEIVED\n" + reply + "\nEXPECTED\n| Chambre: " + strconv.Itoa(i) + ", Status: " + availability)
//		}
//		if i == 8 {
//			availability = "LIBRE"
//		}
//	}
//
//	reply, _ := utils.ReadLn(conn)
//	fmt.Println("Server response :")
//	fmt.Println(reply)
//
//	if reply != "END" {
//		fmt.Println(">> Test ROOMS -> FAIL")
//		t.Error("\nRECEIVED\n" + reply + "\nEXPECTED\nEND")
//	}
//}

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