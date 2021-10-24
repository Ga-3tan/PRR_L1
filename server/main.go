// Package main contains the entry point of the server app
package main

import (
	"hotel/config"
	"hotel/server/logic"
	"hotel/server/serverTcp"
)

// main Entry point of the server program
func main() {
	// Creates the server
	var hotel = logic.Hotel{Reservations: make(map[int][]logic.Reservation), MaxDays: config.MAX_DAYS, MaxRooms: config.MAX_ROOMS}

	// Starts the server
	serverTcp.Start(&hotel)
}
