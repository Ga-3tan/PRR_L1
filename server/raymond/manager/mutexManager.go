package manager

import (
	"container/list"
	"hotel/server/network"
	"hotel/server/raymond"
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
	State			   StateType
	Parent			   int
	Queue			   list.List // doubly linked-list
	AgreedSC           chan struct{}
	ServerMutexCh      chan raymond.MessageType
	MutexConnManagerCh chan raymond.MessageRaymond
	ConnManager        network.ConnManager
}

func (m* MutexManager) front()  {
	if m.Queue.Len() > 0 {
		out := m.Queue.Front()
		m.Queue.Remove(out.Value)
		return out
	}
}