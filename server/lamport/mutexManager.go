package lamport

import (
	"hotel/config"
	"hotel/server/network"
	"log"
	"math"
)

type MutexManager struct {
	SelfId             int
	H                  int
	AgreedSC           chan struct{}
	T                  [config.NB_SERVER]MessageLamport
	ServerMutexCh      chan MessageType
	MutexConnManagerCh chan MessageLamport
	ConnManager        network.ConnManager // TODO à voir si on peut faire sans
}

// TODO main loop and channels for receiving messages from Hotel and ConnManager
func (m MutexManager) Start() {
	for {
		select {
		case srvMsg := <-m.ServerMutexCh:
			if srvMsg == ASK_SC {
				m.askSC()
			} else if srvMsg == END_SC {
				m.relSC()
			} else {
				log.Fatal("MutexProcess: Unknown Server Message")
			}
		case lprtMsg := <-m.MutexConnManagerCh:
			switch lprtMsg.Type {
			case REQ:
				m.req(lprtMsg)
			case ACK:
				m.ack(lprtMsg)
			case REL:
				m.rel(lprtMsg)
			default:
				log.Fatal("MutexProcess: Unknown Lamport Message")
			}
		}
	}
}

// askSC register the REQ and send it to all others servers
func (m MutexManager) askSC() {
	m.H += 1
	lprtMsg := MessageLamport{REQ, m.H, m.SelfId} // TODO selfId necessary ?
	m.T[m.SelfId] = lprtMsg
	m.ConnManager.SendAll(lprtMsg.ToString())
}

// relSC register the REL and send it to all others servers
func (m MutexManager) relSC() {
	m.H += 1
	lprtMsg := MessageLamport{REL, m.H, m.SelfId} // TODO selfId necessary ?
	m.T[m.SelfId] = lprtMsg
	m.ConnManager.SendAll(lprtMsg.ToString())
}

func (m MutexManager) syncStamp(incomingStamp int) {
	m.H = int(math.Max(float64(incomingStamp), float64(m.H))) + 1 // TODO not clean ?
}

func (m MutexManager) req(msg MessageLamport) {
	m.syncStamp(msg.H)
	m.T[msg.SenderID] = msg
	if m.T[m.SelfId].Type != REQ {
		m.ack(msg)
	}
	m.verifySC()
}

func (m MutexManager) ack(msg MessageLamport) {
	m.syncStamp(msg.H)
	if m.T[msg.SenderID].Type != REQ {
		m.T[msg.SenderID] = msg
	}
	m.verifySC()
}

func (m MutexManager) rel(msg MessageLamport) {
	m.syncStamp(msg.H)
	m.T[msg.SenderID] = msg
	m.verifySC()
}

func (m MutexManager) verifySC() {
	if m.T[m.SelfId].Type != REQ {
		return
	}
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
	if oldest { // TODO can be factorised
		m.AgreedSC<- struct{}{}
	}
}
