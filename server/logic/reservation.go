package logic

// Reservation represents a reservation sent by the client
type Reservation struct {
	IdRoom   int
	Client   string
	Day      int
	NbNights int
}
