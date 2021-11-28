// Package config contains all the config constants for the server server
package config

const (
	MAX_DAYS    int    = 20
	MAX_ROOMS          = 10
	DEBUG              = 1 // Set to 1 for debug mode
	DEBUG_SLEEP        = 25
	HOST       string = "localhost"
	NB_SERVERS int    = 3
)

type Server struct{
	Host string
	Port int
}

var Servers = []Server{
	{Host: HOST, Port: 9090},
	{Host: HOST, Port: 9091},
	{Host: HOST, Port: 9092},
}
