// Package config contains all the config constants for the server server
package config

const (
	MAX_DAYS    int    = 20
	MAX_ROOMS          = 10
	DEBUG              = 0 // Set to 1 for debug mode
	DEBUG_SLEEP        = 60
	HOST        string = "localhost"
	NB_SERVERS  int    = 3
)

type Server struct {
	Host     string
	Port     int
	Parent   int
	Siblings []int
}

var Servers = []Server{
	{Host: HOST, Port: 9090, Parent: -1, Siblings: []int{1, 2}}, // 0
	{Host: HOST, Port: 9091, Parent: 0, Siblings: []int{0, 3}},  // 1
	{Host: HOST, Port: 9092, Parent: 0, Siblings: []int{0}},     // 2
	{Host: HOST, Port: 9093, Parent: 1, Siblings: []int{1}},     // 3
}

/*var Servers = []Server{
	{Host: HOST, Port: 9190, Parent: -1, Siblings: []int{1, 2, 3}}, // 0
	{Host: HOST, Port: 9191, Parent: 0, Siblings: []int{0, 4}},     // 1
	{Host: HOST, Port: 9192, Parent: 0, Siblings: []int{0}},        // 2
	{Host: HOST, Port: 9193, Parent: 0, Siblings: []int{0}},        // 3
	{Host: HOST, Port: 9194, Parent: 1, Siblings: []int{1, 5, 6}},  // 4
	{Host: HOST, Port: 9195, Parent: 4, Siblings: []int{4}},        // 5
	{Host: HOST, Port: 9196, Parent: 4, Siblings: []int{4, 7}},     // 6
	{Host: HOST, Port: 9197, Parent: 6, Siblings: []int{6}},        // 7
}*/
