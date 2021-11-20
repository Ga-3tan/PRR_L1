// Package main contains the entry point of the client app
package main

import (
	"hotel/client/clientTcp"
	"log"
	"os"
	"strconv"
)

// main Entry point of the server program
func main() {

	if len(os.Args) < 2 {
		log.Fatal("Veuillez spécifier un numéro pour le serveur")
	}
	srvNo, nbErr := strconv.Atoi(os.Args[1])
	// Errors check
	if nbErr != nil {
		log.Fatal("L'argument donné n'est pas valide" + nbErr.Error())
	}

	// Starts the client
	clientTcp.Start(srvNo)
}
