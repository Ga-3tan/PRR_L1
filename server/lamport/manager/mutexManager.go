package manager

import (
	"hotel/config"
	"hotel/server/lamport"
	"hotel/server/network"
	"log"
	"math"
	"strconv"
)

type MutexManager struct {
	SelfId             int
	H                  int
	AgreedSC           chan struct{}
	T                  [config.NB_SERVER]lamport.MessageLamport
	ServerMutexCh      chan lamport.MessageType
	MutexConnManagerCh chan lamport.MessageLamport
	ConnManager        network.ConnManager
}

func (m *MutexManager) Start() {

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
				log.Fatal("MutexProcess: Unknown Server Message")
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
				log.Fatal("MutexProcess: Unknown Lamport Message")
			}
		}
	}
}

// askSC register the REQ and send it to all others servers
func (m *MutexManager) askSC() {
	log.Println("Sending askSC")
	m.H += 1
	lprtMsg := lamport.MessageLamport{Type: lamport.REQ, H: m.H, SenderID: m.SelfId}
	log.Println("ASK SC msg TYPE: " + lprtMsg.Type)
	m.T[m.SelfId] = lprtMsg
	m.ConnManager.SendAll(lprtMsg.ToString())
}

// relSC register the REL and send it to all others servers
func (m *MutexManager) relSC() {
	log.Println("Sending relSC")
	m.H += 1
	lprtMsg := lamport.MessageLamport{Type: lamport.REL, H: m.H, SenderID: m.SelfId}
	m.T[m.SelfId] = lprtMsg
	m.ConnManager.SendAll(lprtMsg.ToString())
}

func (m *MutexManager) syncStamp(incomingStamp int) {
	m.H = int(math.Max(float64(incomingStamp), float64(m.H))) + 1
}

func (m *MutexManager) req(msg lamport.MessageLamport) {
	log.Println("Receiving req")
	m.syncStamp(msg.H)
	m.T[msg.SenderID] = msg
	if m.T[m.SelfId].Type != lamport.REQ {
		lprtMsg := lamport.MessageLamport{Type: lamport.ACK, H: m.H, SenderID: m.SelfId}
		log.Println(lprtMsg.ToString() + "   " + strconv.Itoa(msg.SenderID))
		m.ConnManager.SendTo(msg.SenderID, lprtMsg.ToString())
	}
	m.verifySC()
}

func (m *MutexManager) ack(msg lamport.MessageLamport) {
	log.Println("Receiving ack")
	m.syncStamp(msg.H)
	if m.T[msg.SenderID].Type != lamport.REQ {
		m.T[msg.SenderID] = msg
	}
	m.verifySC()
}

func (m *MutexManager) rel(msg lamport.MessageLamport) {
	log.Println("Receiving rel")
	m.syncStamp(msg.H)
	m.T[msg.SenderID] = msg
	m.verifySC()
}

func (m *MutexManager) verifySC() {
	log.Println("VerifySC access\nLOCAL LAMPORT ARRAY : | ")
	for _, msg := range m.T {
		log.Print(msg.Type + " | ")
	}
	log.Println()

	// Return when not REQ message
	if m.T[m.SelfId].Type != lamport.REQ {
		log.Println("VerifySC access denied")
		return
	}

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
		log.Println("VerifySC access granted")
		m.AgreedSC<- struct{}{}
	}
	log.Println("VerifySC access denied")
}
