package cmd

// CommandType represents all available commands managed by the server
type CommandType string
const (
	BOOK  CommandType = "BOOK"
	ROOMS             = "ROOMS"
	FREE	 		  = "FREE"
	STOP			  = "STOP"
)