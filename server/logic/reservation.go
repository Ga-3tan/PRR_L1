package logic

import "strconv"

// Reservation represents a reservation sent by the client
type Reservation struct {
	IdRoom   int
	Client   string
	Day      int
	NbNights int
}

func (r *Reservation) ToString() string {
	return strconv.Itoa(r.IdRoom) + " " + strconv.Itoa(r.Day) + " " + strconv.Itoa(r.NbNights)
}