// Package manager contains the server logic bloc
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

// CommandManager One of the three main components of the program, represents the server logic bloc
type CommandManager struct {
	Hotel *logic.Hotel
	CmdChan chan cmd.Command
	CmdSyncChan chan cmd.SyncCommand
	ServerMutexCh chan lamport.MessageType
	AgreedSC chan struct{}
	ConnManager network.ConnManager
}

// HandleCommands Handles concurrency and executes the new given commands
func (m *CommandManager) HandleCommands() {
	// Checks if in debug mode
	if config.DEBUG != 0 {
		// Sleeps for n seconds
		log.Println("DEBUG>> Server started in debug mode, now sleeping for " + strconv.Itoa(config.DEBUG_SLEEP) + " seconds ...")
		time.Sleep(config.DEBUG_SLEEP * time.Second)
		log.Println("DEBUG>> " + strconv.Itoa(config.DEBUG_SLEEP) + " seconds sleep done, checking for incoming requests")
	}

	for newCmd := range m.CmdChan {
		var res string
		var err error
		switch newCmd.Cmd {
		case cmd.BOOK:
			log.Println("CommandManager>> New BOOK command from " + newCmd.Reservation.Client)
			m.ServerMutexCh<- lamport.ASK_SC
			<-m.AgreedSC // Wait for signal from agreedSC

			res, err = m.Hotel.BookRoom(newCmd.Reservation.IdRoom, newCmd.Reservation.Day, newCmd.Reservation.NbNights, newCmd.Reservation.Client)

			// Propagates the new command and releases the SC
			log.Println("CommandManager>> Propagating new BOOK command to server pool")
			m.ConnManager.SendAll(newCmd.ToSyncStringCommand(m.ConnManager.Id))
			<-m.ConnManager.WaitSyncCh // Waits for sync to complete
			log.Println("CommandManager>> Sync complete, ready to release critical section")
			m.ServerMutexCh<- lamport.END_SC
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

		if err != nil {
			newCmd.ReturnContent <- err.Error()
		} else {
			newCmd.ReturnContent <- res
		}
	}
}

// HandleSyncCommands handles commands received from another server and syncs them in the reservations list
func (m *CommandManager) HandleSyncCommands() {
	for syncCmd := range m.CmdSyncChan {
		switch syncCmd.Command.Cmd {
		case cmd.BOOK:
			log.Println("CommandManager>> Syncing BOOK command from " + syncCmd.Command.Reservation.Client)
			_, _ = m.Hotel.BookRoom(syncCmd.Command.Reservation.IdRoom, syncCmd.Command.Reservation.Day, syncCmd.Command.Reservation.NbNights, syncCmd.Command.Reservation.Client)
			log.Println("CommandManager>> Synced command, notifies " + strconv.Itoa(syncCmd.AuthorId))
			m.ConnManager.SendTo(syncCmd.AuthorId, string(network.SYNC))
		default:
			log.Println("CommandManager>> Unknown sync command received")
		}

	}
}
