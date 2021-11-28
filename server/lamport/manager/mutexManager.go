// Package manager contains the Lamport algorithm bloc
package manager

import (
	"hotel/config"
	"hotel/server/lamport"
	"hotel/server/network"
	"log"
	"math"
	"strconv"
)

// MutexManager One of the three main components of the program, represents the Lamport algorithm bloc
type MutexManager struct {
	SelfId             int
	H                  int
	AgreedSC           chan struct{}
	T                  [config.NB_SERVERS]lamport.MessageLamport
	ServerMutexCh      chan lamport.MessageType
	MutexConnManagerCh chan lamport.MessageLamport
	ConnManager        network.ConnManager
	inSC			   bool
}

// Start Initiates the Lamport algorithm and blocks on the Lamport channel
func (m *MutexManager) Start() {
	// Not in SC at start
	m.inSC = false;

	// Init Lamport array with REL 0 values
	for i := 0; i < len(m.T); i++ {
		m.T[i] = lamport.MessageLamport{
			Type:     lamport.REL,
			H:        0,
			SenderID: i,
		}
	}

	// Main loop
	for {
		select {
		case srvMsg := <-m.ServerMutexCh:
			if srvMsg == lamport.ASK_SC {
				m.askSC()
			} else if srvMsg == lamport.END_SC {
				m.relSC()
			} else {
				log.Fatal("MutexManager>> Unknown Server Message")
			}
		case lprtMsg := <-m.MutexConnManagerCh:
			switch lprtMsg.Type {
			case lamport.REQ:
				m.req(lprtMsg)
			case lamport.ACK:
				m.ack(lprtMsg)
			case lamport.REL:
				m.rel(lprtMsg)
			default:
				log.Fatal("MutexManager>> Unknown Lamport Message")
			}
		}
	}
}

// syncStamp Updates the Lamport stamp with the right value
func (m *MutexManager) syncStamp(incomingStamp int) {
	m.H = int(math.Max(float64(incomingStamp), float64(m.H))) + 1
}

// askSC Asks for the critical section, notifies all by sending a REQ message
func (m *MutexManager) askSC() {
	log.Println("MutexManager>> Sending REQ to all")
	m.H += 1
	lprtMsg := lamport.MessageLamport{Type: lamport.REQ, H: m.H, SenderID: m.SelfId}
	m.T[m.SelfId] = lprtMsg
	m.ConnManager.SendAll(lprtMsg.ToString())
}

// relSC Exits the critical section, notifies all by sending a REL message
func (m *MutexManager) relSC() {
	log.Println("MutexManager>> Sending REL to all")
	m.H += 1
	lprtMsg := lamport.MessageLamport{Type: lamport.REL, H: m.H, SenderID: m.SelfId}
	m.T[m.SelfId] = lprtMsg
	m.inSC = false
	log.Println("MutexManager>> ------------- EXITING CRITICAL SECTION -------------")
	m.ConnManager.SendAll(lprtMsg.ToString())
}

// req Called when a new REQ message is received
func (m *MutexManager) req(msg lamport.MessageLamport) {
	log.Println("MutexManager>> Received REQ from " + strconv.Itoa(msg.SenderID))
	m.syncStamp(msg.H)
	m.T[msg.SenderID] = msg
	if m.T[m.SelfId].Type != lamport.REQ {
		lprtMsg := lamport.MessageLamport{Type: lamport.ACK, H: m.H, SenderID: m.SelfId}
		log.Println("MutexManager>> Sending ACK to " + strconv.Itoa(msg.SenderID))
		m.ConnManager.SendTo(msg.SenderID, lprtMsg.ToString())
	}
	m.verifySC()
}

// ack Called when a new ACK message is received
func (m *MutexManager) ack(msg lamport.MessageLamport) {
	log.Println("MutexManager>> Received ACK from " + strconv.Itoa(msg.SenderID))
	m.syncStamp(msg.H)
	if m.T[msg.SenderID].Type != lamport.REQ {
		m.T[msg.SenderID] = msg
	}
	m.verifySC()
}

// rel Called when a new REL message is received
func (m *MutexManager) rel(msg lamport.MessageLamport) {
	log.Println("MutexManager>> Received REL from " + strconv.Itoa(msg.SenderID))
	m.syncStamp(msg.H)
	m.T[msg.SenderID] = msg
	m.verifySC()
}

// verifySC Checks if the critical section can be accessed
func (m *MutexManager) verifySC() {
	// Array log
	out := "MutexManager>> Array state : | "
	for _, msg := range m.T {
		out += strconv.Itoa(msg.SenderID) + "->" + string(msg.Type) + " " + strconv.Itoa(msg.H) + " | "
	}
	log.Println(out)

	// Return when not REQ message
	if m.T[m.SelfId].Type != lamport.REQ {
		return
	}

	log.Println("MutexManager>> Critical Section : need access, verifying access")

	// Applies Lamport Logical clock algorithm
	oldest := true
	for _, msg := range m.T {
		if msg.SenderID == m.SelfId {
			continue
		}
		if (m.T[m.SelfId].H > msg.H) || (m.T[m.SelfId].H == msg.H && m.SelfId > msg.SenderID) {
			oldest = false
			break
		}
	}

	// Checks SC whether access is granted or not
	if oldest {
		log.Println("MutexManager>> Critical Section : Access granted")
		if !m.inSC {
			m.inSC = true
			log.Println("MutexManager>> ------------- ENTERING CRITICAL SECTION -------------")
			m.AgreedSC<- struct{}{}
		}
	} else {
		log.Println("MutexManager>> Critical Section : Access denied")
	}
}
