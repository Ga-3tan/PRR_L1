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

type CommandManager struct {
	Hotel *logic.Hotel
	CmdChan chan cmd.Command
	ServerMutexCh chan lamport.MessageType
	AgreedSC chan struct{}
	ConnManager network.ConnManager
}

// CommandHandler Handles concurrency and executes the given commands
func (m *CommandManager) HandleCommands() { // TODO Too much arguments ? Maybe make a struct "CommandManager"
	// Checks if in debug mode"
	if config.DEBUG != 0 {
		// Sleeps for n seconds
		log.Println("CommandManager>> DEBUG Server started in debug mode, now sleeping for " + strconv.Itoa(config.DEBUG_SLEEP) + " seconds ...")
		time.Sleep(config.DEBUG_SLEEP * time.Second)
		log.Println("CommandManager>> DEBUG " + strconv.Itoa(config.DEBUG_SLEEP) + " seconds sleep done, checking for incoming requests")
	}

	for newCmd := range m.CmdChan {
		var res string
		var err error
		switch newCmd.Cmd {
		case cmd.BOOK:
			// Checks whether it is a sync or a new command
			if !newCmd.SyncCmd { // AskSC and Wait only if it's not a sync command
				log.Println("CommandManager>> New BOOK command from " + newCmd.Reservation.Client)
				m.ServerMutexCh<- lamport.ASK_SC
				<-m.AgreedSC // Wait for signal from agreedSC
			} else {
				log.Println("CommandManager>> Syncing BOOK command from " + newCmd.Reservation.Client)
			}

			res, err = m.Hotel.BookRoom(newCmd.Reservation.IdRoom, newCmd.Reservation.Day, newCmd.Reservation.NbNights, newCmd.Reservation.Client)

			// Propagates the new command and releases the SC if it was a new command
			if !newCmd.SyncCmd { // Sync and EndSC only if it's not a sync command
				log.Println("CommandManager>> Propagating new BOOK command to server pool")
				m.ConnManager.SendAll(newCmd.ToSyncStringCommand())
				m.ServerMutexCh<- lamport.END_SC
			}
		case cmd.ROOMS:
			log.Println("CommandManager>> New ROOMS command received")
			res, err = m.Hotel.GetRoomsList(newCmd.Reservation.Day, newCmd.Reservation.Client)
		case cmd.FREE:
			log.Println("CommandManager>> New FREE command received")
			res, err = m.Hotel.GetFreeRoom(newCmd.Reservation.Day, newCmd.Reservation.NbNights)
		default:
			log.Println("CommandManager>> Unknown command received")
			err = errors.New("CommandManager>> ERR Erreur de commande")
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
