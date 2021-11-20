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
	ConnManager        network.ConnManager // TODO à voir si on peut faire sans
}

// TODO main loop and channels for receiving messages from Hotel and ConnManager
func (m MutexManager) Start() {
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
func (m MutexManager) askSC() {
	println("askSC")
	m.H += 1
	lprtMsg := lamport.MessageLamport{lamport.REQ, m.H, m.SelfId} // TODO selfId necessary ?
	println("ASK SC msg TYPE: " + lprtMsg.Type)
	m.T[m.SelfId] = lprtMsg
	m.ConnManager.SendAll(lprtMsg.ToString())
}

// relSC register the REL and send it to all others servers
func (m MutexManager) relSC() {
	println("relSC")
	m.H += 1
	lprtMsg := lamport.MessageLamport{Type: lamport.REL, H: m.H, SenderID: m.SelfId} // TODO selfId necessary ?
	m.T[m.SelfId] = lprtMsg
	m.ConnManager.SendAll(lprtMsg.ToString())
}

func (m MutexManager) syncStamp(incomingStamp int) {
	m.H = int(math.Max(float64(incomingStamp), float64(m.H))) + 1 // TODO not clean ?
}

func (m MutexManager) req(msg lamport.MessageLamport) {
	println("req")
	m.syncStamp(msg.H)
	m.T[msg.SenderID] = msg
	if m.T[m.SelfId].Type != lamport.REQ {
		lprtMsg := lamport.MessageLamport{Type: lamport.ACK, H: m.H, SenderID: m.SelfId} // TODO selfId necessary ?
		println(lprtMsg.ToString() + "   " + strconv.Itoa(msg.SenderID))
		m.ConnManager.SendTo(msg.SenderID, lprtMsg.ToString())
	}
	m.verifySC()
}

func (m MutexManager) ack(msg lamport.MessageLamport) {
	println("ack")
	m.syncStamp(msg.H)
	if m.T[msg.SenderID].Type != lamport.REQ {
		m.T[msg.SenderID] = msg
	}
	m.verifySC()
}

func (m MutexManager) rel(msg lamport.MessageLamport) {
	println("rel")
	m.syncStamp(msg.H)
	m.T[msg.SenderID] = msg
	m.verifySC()
}

func (m MutexManager) verifySC() {
	println("VERIFIY SC " + m.T[m.SelfId].Type)
	if m.T[m.SelfId].Type != lamport.REQ {
		println("VERIFIY SC FAILED")
		return
	}
	oldest := true
	for _, msg := range m.T {
		if msg.SenderID == m.SelfId {
			println("VERIFIY SC CONTINUED")
			continue
		}
		if (m.T[m.SelfId].H > msg.H) || (m.T[m.SelfId].H == msg.H && m.SelfId > msg.SenderID) {
			println("VERIFIY SC BREAK")
			oldest = false
			break
		}
	}
	if oldest { // TODO can be factorised
		println("VERIFIY SC SUCCSESS")
		m.AgreedSC<- struct{}{}
	}
	println("VERIFIY SC FAILED")
}
