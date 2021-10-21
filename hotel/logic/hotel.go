// Package logic contains all the hotel logic and reservations interactions
package logic

import (
	"errors"
	"strconv"
)

// Hotel represents a hotel structure
type Hotel struct {
	Reservations map[int][]Reservation
	MaxDays  	 int
	MaxRooms 	 int
}

// isAlreadyBooked verifies whether a room is already booked or not.
//Returns a boolean and the overlapping reservation if one has been found
func (hotel *Hotel) isAlreadyBooked(idRoom int, day int, nbNights int) (bool, Reservation) {
	lastDay := day + nbNights - 1 // jour avant le check-out

	// Pour chaque réservation de la chambre, on vérifie si le premier et
	// le dernier jour ne sont pas dans les bornes d'une autre réservation
	for _, res := range hotel.Reservations[idRoom] {
		resLastDay := res.Day + res.NbNights - 1
		if (day >= res.Day && day <= resLastDay) || (lastDay >= res.Day && lastDay <= resLastDay) {
			return true, res
		}
	}
	return false, Reservation{}
}

// BookRoom books a room in the given hotel
func (hotel *Hotel) BookRoom(idRoom int, day int, nbNights int, client string) (string, error) {
	if day < 1 || day > hotel.MaxDays {
		return "", errors.New("ERR jour invalide")
	} else if idRoom < 1 || idRoom > hotel.MaxRooms {
		return "", errors.New("ERR chambre invalide")
	}

	isBooked, _ := hotel.isAlreadyBooked(idRoom, day, nbNights)

	if isBooked {
		return "", errors.New("ERR votre chambre est déjà réservée")
	}

	hotel.Reservations[idRoom] = append(hotel.Reservations[idRoom], Reservation{IdRoom: idRoom, Client: client, Day: day, NbNights: nbNights})
	return "OK votre chambre a été réservée", nil
}

// GetRoomsList retrieves a list of rooms and their status
func (hotel *Hotel) GetRoomsList(day int, clientName string) (string, error) {
	ret := ""

	for i := 1; i <= hotel.MaxRooms; i++ {
		ret += "| Chambre: " + strconv.Itoa(i) + ", Status: "

		// Vérifier si le jour "day" est réservé pour 1 nuit correspond à vérifier si ce jour là est libre ou pas
		isBooked, res := hotel.isAlreadyBooked(i, day, 1)
		if isBooked {
			if clientName == res.Client {
				ret += "RESERVE"
			} else {
				ret += "OCCUPE"
			}
		} else {
			ret += "LIBRE"
		}
		ret += "\n"
	}
	ret += "END\n"
	return ret, nil
}

// GetFreeRoom finds an available room with the given arguments
func (hotel *Hotel) GetFreeRoom(day int, nbNights int) (string, error) {
	for i := 1; i <= hotel.MaxRooms; i++ {
		isBooked, _ := hotel.isAlreadyBooked(i, day, nbNights)
		if !isBooked {
			return "OK chambre " + strconv.Itoa(i) + " disponible", nil
		}
	}
	return "", errors.New("ERR aucune chambre disponible")
}
