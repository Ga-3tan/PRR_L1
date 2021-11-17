// Package main contains the entry point of the server app
package main

import (
	"hotel/config"
	"hotel/server/logic"
	"hotel/server/serverTcp"
	"log"
	"os"
	"strconv"
)

// main Entry point of the server program
func main() {
	// Retrieves program arguments
		if len(os.Args) < 2 {
			log.Fatal("Veuillez spécifier un numéro pour le serveur")
		}
		srvNb, nbErr := strconv.Atoi(os.Args[1])
		// Errors check
		if nbErr != nil {
			log.Fatal("L'argument donné n'est pas valide" + nbErr.Error())
		}

	// Creates the server
	var hotel = logic.Hotel{Reservations: make(map[int][]logic.Reservation), MaxDays: config.MAX_DAYS, MaxRooms: config.MAX_ROOMS}

	// Starts the server
	serverTcp.Start(srvNb, &hotel)
}
