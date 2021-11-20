package logic

import "strconv"

// Reservation represents a reservation sent by the client
type Reservation struct {
	IdRoom   int
	Client   string
	Day      int
	NbNights int
}

// TODO CAN BE SIMPLIFIED
/*  not very useful bc the server only sync BOOK so we can return only all properties */
func (r Reservation) ToString() string {
	s := ""
	if r.IdRoom != 0 {
		s += strconv.Itoa(r.IdRoom)
		if r.Day != 0 {
			s += " "
		}
	}
	if r.Day != 0 {
		s += strconv.Itoa(r.Day)
		if r.NbNights != 0 {
			s += " "
		}
	}
	if r.NbNights != 0 {
		s += strconv.Itoa(r.NbNights)
	}
	return s
}