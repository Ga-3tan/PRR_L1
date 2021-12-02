// Package config contains all the config constants for the server server
package config

const (
	MAX_DAYS    int    = 20
	MAX_ROOMS          = 10
	DEBUG              = 0 // Set to 1 for debug mode
	DEBUG_SLEEP        = 60
	HOST       string = "localhost"
	NB_SERVERS int    = 3
)

type Server struct{
	Host string
	Port int
	Parent int
	Siblings []int
}

var Servers = []Server{
	{Host: HOST, Port: 9090, Parent: -1, Siblings: []int{1, 2}},	// 0
	{Host: HOST, Port: 9091, Parent: 0, Siblings: []int{0, 3}},		// 1
	{Host: HOST, Port: 9092, Parent: 0, Siblings: []int{0}},		// 2
	{Host: HOST, Port: 9093, Parent: 1, Siblings: []int{1}},		// 3
}
