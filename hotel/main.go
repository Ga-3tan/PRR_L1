package main

import (
	"hotel/hotel/config"
	"hotel/hotel/logic"
	"hotel/hotel/net"
)

/**
Entry point for the hotel app
*/
func main() {
	/*/ Retrieves program arguments
	debugMode := false
	if len(os.Args) < 3 {
		log.Fatal("Nombre d'arguments invalide : <nbMaxJours> <nbMaxChambres> <modeDebug>")
	} else if len(os.Args) == 4 && os.Args[3] == "debug" {
		debugMode = true
	}

	maxDays, daysErr := strconv.Atoi(os.Args[1])
	maxRooms, roomsErr := strconv.Atoi(os.Args[2])

	// Errors check
	if daysErr != nil || roomsErr != nil {
		log.Fatal("Les arguments donnés ne sont pas valides" + daysErr.Error())
	}

	// Reads the config file
	conf, err := os.ReadFile("./hotel.conf")

	if err != nil {
		log.Fatal("Aucun fichier de configuration hotel.conf trouvé : " + err.Error())
	}*/

	// Creates the hotel
	var hotel = logic.Hotel{Reservations: make(map[int][]logic.Reservation), MaxDays: config.MAX_DAYS, MaxRooms: config.MAX_ROOMS}

	// Starts the server
	net.Start(&hotel)
}
