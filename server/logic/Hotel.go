package logic

type Hotel struct {
	rooms [][]Reservation
}

func (hotel *Hotel) BookRoom(idRoom uint8, day uint16, nbNights uint16) string {
	return "marco"
}

func GetRoomsList() string {
	return "marco"
}

func GetFreeRoom(day uint16, nbNights uint16) string {
	return "ocram"
}