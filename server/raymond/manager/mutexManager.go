package manager

import (
	"hotel/server/network"
	"hotel/server/raymond"
	"log"
	"strconv"
)

type StateType string

const (
	S StateType = "S"
	N StateType = "N"
	D StateType = "D"
)

// MutexManager One of the three main components of the program, represents the Raymond algorithm bloc
type MutexManager struct {
	SelfId             int
	State              StateType
	Parent             int
	Queue              []int
	AgreedSC           chan struct{}
	ServerMutexCh      chan raymond.MessageType
	MutexConnManagerCh chan raymond.MessageRaymond
	ConnManager        *network.ConnManager
	needAccessSC       bool
}

func (m *MutexManager) queuePop() int {
	if len(m.Queue) > 0 {
		out := m.Queue[0]
		m.Queue = m.Queue[1:]
		return out
	}
	return -1
}

func (m *MutexManager) queuePush(value int) {
	m.Queue = append(m.Queue, value)
}

func (m *MutexManager) queueIsEmpty() bool {
	return len(m.Queue) == 0
}

func (m *MutexManager) sendReqToParent() {
	newReq := raymond.MessageRaymond{Type: raymond.REQ, SenderID: m.SelfId}
	m.ConnManager.SendTo(m.Parent, newReq.ToString())
}

func (m *MutexManager) sendTokToParent() {
	newTok := raymond.MessageRaymond{Type: raymond.TOK, SenderID: m.SelfId}
	m.ConnManager.SendTo(m.Parent, newTok.ToString())
}

// Start Initiates the Lamport algorithm and blocks on the Lamport channel
func (m *MutexManager) Start() {
	// Main loop
	for {
		select {
		case srvMsg := <-m.ServerMutexCh:
			if srvMsg == raymond.ASK_SC {
				m.askSC()
			} else if srvMsg == raymond.END_SC {
				m.relSC()
			} else {
				log.Fatal("MutexManager>> Unknown Server Message")
			}
		case raymMsg := <-m.MutexConnManagerCh:
			switch raymMsg.Type {
			case raymond.REQ:
				m.req(raymMsg)
			case raymond.TOK:
				m.tok(raymMsg)
			default:
				log.Fatal("MutexManager>> Unknown Raymond Message")
			}
		}
	}
}

func (m *MutexManager) askSC() {
	log.Println("MutexManager>> ############# ASKING FOR CRITICAL SECTION #############")

	if m.Parent != -1 {
		m.queuePush(m.SelfId)

		if m.State == N {
			m.State = D
			log.Println("MutexManager>> Sending REQ to parent " + strconv.Itoa(m.Parent))
			m.sendReqToParent()
		}
	}

	// Sets need access
	m.needAccessSC = true

	// Checks if SC is wanted and can access
	m.enterSC()
}

func (m *MutexManager) relSC() {
	m.State = N
	if !m.queueIsEmpty() {
		m.Parent = m.queuePop() // Changes arc direction

		log.Println("MutexManager>> Sending TOK to parent " + strconv.Itoa(m.Parent))
		m.sendTokToParent()

		if !m.queueIsEmpty() {
			m.State = D
			log.Println("MutexManager>> Sending REQ to parent " + strconv.Itoa(m.Parent))
			m.sendReqToParent()
		}
	}

	log.Println("MutexManager>> ------------- EXITING CRITICAL SECTION -------------")
}

func (m *MutexManager) req(msg raymond.MessageRaymond) {
	log.Println("MutexManager>> Received REQ from " + strconv.Itoa(msg.SenderID))
	if m.Parent == -1 && m.State == N {
		m.Parent = msg.SenderID // Changes arc direction

		log.Println("MutexManager>> Sending TOK to parent " + strconv.Itoa(m.Parent))
		m.sendTokToParent()
	} else {
		m.queuePush(msg.SenderID)
		if m.Parent != -1 && m.State == N {
			m.State = D

			log.Println("MutexManager>> Sending REQ to parent " + strconv.Itoa(m.Parent))
			m.sendReqToParent()
		}
	}
}

func (m *MutexManager) tok(msg raymond.MessageRaymond) {
	log.Println("MutexManager>> Received TOK from " + strconv.Itoa(msg.SenderID))

	m.Parent = m.queuePop()

	if m.Parent == m.SelfId {
		m.Parent = -1
	} else {
		log.Println("MutexManager>> Sending TOK to parent " + strconv.Itoa(m.Parent))
		m.sendTokToParent()

		if !m.queueIsEmpty() {
			m.State = D

			log.Println("MutexManager>> Sending REQ to parent " + strconv.Itoa(m.Parent))
			m.sendReqToParent()
		} else {
			m.State = N
		}
	}

	// Checks if SC is wanted and can access
	m.enterSC()
}

func (m *MutexManager) enterSC() {
	log.Println("MutexManager>> Critical Section : verifying access")
	if m.needAccessSC == true && m.Parent == -1 {
		m.needAccessSC = false
		log.Println("MutexManager>> Critical Section : Access granted")
		log.Println("MutexManager>> ------------- ENTERING CRITICAL SECTION -------------")
		m.State = S
		m.AgreedSC <- struct{}{}
	} else {
		log.Println("MutexManager>> Critical Section : Access denied")
	}
}
