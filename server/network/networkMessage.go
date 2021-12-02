package network

// MessageType The type of Lamport related messages
type NetworkMessage string

const (
	SRV	 NetworkMessage = "SRV"
	CLI  NetworkMessage = "CLI"
	SYNC NetworkMessage = "SYOK"
	STRT NetworkMessage = "STRT"
	LPRT NetworkMessage = "LPRT"
	RDY  NetworkMessage = "RDY"
)
