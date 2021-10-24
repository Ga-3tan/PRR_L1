package main

import (
	"bufio"
	"fmt"
	"hotel/cmd"
	"log"
	"net"
	"os"
	"strings"
)

//func readLineUntil(str string, conn net.Conn) string { // TODO check this return
func readLineUntil(str string, scanner* bufio.Scanner) string { // TODO check this return
	reply := readLine(scanner)
	for !strings.Contains(reply, str) {
		reply += readLine(scanner)
	}
	return reply
}

//func readLine(conn net.Conn) string {
func readLine(scanner* bufio.Scanner) string {
	//reply := make([]byte, 1024)
	//_, err := conn.Read(reply)
	scanner.Scan()
	return scanner.Text()
	//if err != nil {
	//	println("Read to server failed:", err.Error())
	//	os.Exit(1)
	//}
	//return string(reply)
}

func writelnStr(str string, conn* net.Conn) {
	_, err := fmt.Println(*conn, str)
	if err != nil {
		println("Write to server failed:", err.Error())
		os.Exit(1)
	}
}

func Connect() net.Conn {
	// Connects to the hotel on localhost:8000
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
	conn := Connect()
	defer Disconnect(conn)

	consoleReader := bufio.NewReader(os.Stdin)
	//fmt.Print("Entrez votre nom: ")
	//username, err := consoleReader.ReadString('\n')
	//if err != nil {
	//	log.Fatal(err)
	//}
	//fmt.Println("Bonjour " + username + ", entrez vos requêtes: ")

	var req string
	var reply string

	scanner := bufio.NewScanner(conn)

	reply = readLineUntil("Veuillez spécifier votre nom :", scanner)
	fmt.Print(reply)

	for reply != cmd.STOP {
		req, _ = consoleReader.ReadString('\n')
		fmt.Println("request: " + req)
		writelnStr(req, &conn)
		reply = readLine(scanner)
		fmt.Println("reply : " + reply)
	}
}
