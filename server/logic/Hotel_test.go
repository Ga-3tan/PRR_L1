package logic

import (
	"testing"
)

func TestBookRoom(t *testing.T) {
	// reservations

	// room 1 [0-15[
	r1 := Reservation{ client: "AAA", day: 0, nbNights: 5} 	// day [0-5[
	r2 := Reservation{ client: "BBB", day: 5, nbNights: 10} // day [5-15[
	// room 2 [3-18[ U [19-20[
	r3 := Reservation{ client: "CCC", day: 3, nbNights: 15} // day [3-18[
	r4 := Reservation{ client: "DDD", day: 19, nbNights: 2} // day [19-20[
	r6 := Reservation{ client: "FFF", day: 18, nbNights: 1}	// day [18-19[
	// room 3 [1-22[
	r5 := Reservation{ client: "EEE", day: 1, nbNights: 21}	// day [1-22[

	var hotel Hotel

	// valid booking
	_, err1 := hotel.BookRoom(1, 0, 5, "AAA") // room 1, day [0-5[
	if err1 == nil || len(hotel.Reservations) != 1 || hotel.Reservations[0] != r1 {
		t.Error("First reservation not registered")
	}
	_, err2 := hotel.BookRoom(1,5,10, "BBB")
	if err2 == nil || len(hotel.Reservations) != 2 || hotel.Reservations[1] != r2 {
		t.Error("Second reservation not registered")
	}
	_, err3 := hotel.BookRoom(2, 3,15, "CCC")
	if err3 == nil || len(hotel.Reservations) != 3 || hotel.Reservations[2] != r3 {
		t.Error("Third reservation not registered")
	}
	_, err4 := hotel.BookRoom(2, 19, 2, "DDD")
	if err4 == nil || len(hotel.Reservations) != 4 || hotel.Reservations[3] != r4 {
		t.Error("Fourth reservation not registered")
	}
	_, err5 := hotel.BookRoom(3, 1, 21, "DDD")
	if err5 == nil || len(hotel.Reservations) != 5 || hotel.Reservations[4] != r5 {
		t.Error("Fifth reservation not registered")
	}

	// invalid booking
	_, err6 := hotel.BookRoom(1, 3, 5, "ABC")
	if err6 != nil {
		t.Error("Sixth reservation should not be registered")
	}
	_, err7 := hotel.BookRoom(2, 3, 5, "DEF")
	if err7 != nil {
		t.Error("Seventh reservation should not be registered")
	}
	_, err8 := hotel.BookRoom(3, 3, 5, "GHI")
	if err8 != nil {
		t.Error("Eigth reservation should not be registered")
	}

	// test booking between 2 booking
	_, err9 := hotel.BookRoom(2, 18, 1, "FFF")
	if err9 == nil || len(hotel.Reservations) != 4 || hotel.Reservations[5] != r6 {
		t.Error("Ninth reservation not registered")
	}
}