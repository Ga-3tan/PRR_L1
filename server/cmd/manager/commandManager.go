package manager

import (
	"errors"
	"hotel/config"
	"hotel/server/cmd"
	"hotel/server/lamport"
	"hotel/server/logic"
	"hotel/server/network"
	"log"
	"strconv"
	"time"
)

// CommandHandler Handles concurrency and executes the given commands
func CommandHandler(hotel *logic.Hotel, cmdChan chan cmd.Command, serverMutexCh chan<- lamport.MessageType,  agreedSC <-chan struct{}, connManager network.ConnManager) { // TODO Too much arguments ? Maybe make a struct "CommandManager"
	// Checks if in debug mode"
	if config.DEBUG != 0 {
		// Sleeps for n seconds
		log.Println("DEBUG Server started in debug mode, now sleeping for " + strconv.Itoa(config.DEBUG_SLEEP) + " seconds ...")
		time.Sleep(config.DEBUG_SLEEP * time.Second)
		log.Println("DEBUG " + strconv.Itoa(config.DEBUG_SLEEP) + " seconds sleep done, checking for incoming requests")
	}

	for newCmd := range cmdChan {
		var res string
		var err error
		switch newCmd.Cmd {
		case cmd.BOOK:
			log.Println("LOG BOOK command received from " + newCmd.Reservation.Client)
			if !newCmd.SyncCmd { // AskSC and Wait only if it's not a sync command
				serverMutexCh<- lamport.ASK_SC
				<-agreedSC // Wait for signal from agreedSC
			}
			res, err = hotel.BookRoom(newCmd.Reservation.IdRoom, newCmd.Reservation.Day, newCmd.Reservation.NbNights, newCmd.Reservation.Client)
			if !newCmd.SyncCmd { // Sync and EndSC only if it's not a sync command
				connManager.SendAll(newCmd.ToSyncStringCommand())
				serverMutexCh<- lamport.END_SC
			}
		case cmd.ROOMS:
			log.Println("LOG ROOMS command received")
			res, err = hotel.GetRoomsList(newCmd.Reservation.Day, newCmd.Reservation.Client)
		case cmd.FREE:
			log.Println("LOG FREE command received")
			res, err = hotel.GetFreeRoom(newCmd.Reservation.Day, newCmd.Reservation.NbNights)
		default:
			log.Println("LOG Unknown command received")
			err = errors.New("ERR Erreur de commande")
		}

		if !newCmd.SyncCmd {
			if err != nil {
				newCmd.ReturnContent <- err.Error()
			} else {
				newCmd.ReturnContent <- res
			}
		}
	}
}
