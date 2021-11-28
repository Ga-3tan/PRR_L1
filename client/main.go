// Package main contains the entry point of the client app
package main

import (
	"hotel/client/clientTcp"
	"hotel/config"
	"log"
	"math/rand"
	"os"
	"strconv"
	"time"
)

// main Entry point of the server program
func main() {

	// Generates random server number
	rand.Seed(time.Now().UnixNano())
	srvNo := rand.Intn(config.NB_SERVERS)

	if len(os.Args) >= 2 {
		parsedNb, nbErr := strconv.Atoi(os.Args[1])
		// Errors check
		if nbErr != nil {
			log.Fatal("L'argument donné n'est pas valide" + nbErr.Error())
		}
		srvNo = parsedNb
	}

	// Log
	log.Println("Le client va se connecter au serveur no. " + strconv.Itoa(srvNo))

	// Starts the client
	clientTcp.Start(srvNo)
}
