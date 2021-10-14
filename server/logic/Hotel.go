package logic

import (
	"strconv"
)

type Hotel struct {
	Reservations []Reservation
}

var HOTEL = Hotel{Reservations: []Reservation{}}

func (hotel *Hotel) BookRoom(idRoom int, day int, nbNights int, client string) (string, error) {
	hotel.Reservations = append(hotel.Reservations, Reservation{idRoom: idRoom, client: client, day: day, nbNights: nbNights})
	return "Reservation ajoutée", nil
}

func (hotel *Hotel) GetRoomsList() (string, error) {
	ret := ""

	for _, r := range hotel.Reservations {
		ret = ret + "| Chambre: " + strconv.Itoa(r.idRoom) + ", jour: " + strconv.Itoa(r.day) + ", nuits: " + strconv.Itoa(r.nbNights) + " |\n"
	}
	return ret, nil
}

func GetFreeRoom(day uint16, nbNights uint16) string {
	return "ocram"
}