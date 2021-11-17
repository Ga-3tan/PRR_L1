// Package utils contains util functions used in the program
package utils

import (
	"bufio"
	"bytes"
	"io"
	"log"
	"net"
)

// ReadLn is used to read a line from a source connexion
func ReadLn(conn net.Conn) string {
	reader := bufio.NewReader(conn)
	var buffer bytes.Buffer
	for {
		ba, isPrefix, err := reader.ReadLine()
		if err != nil {
			if err == io.EOF {
				break
			}
			log.Fatal(err)
		}
		buffer.Write(ba)
		if !isPrefix {
			break
		}
	}
	return buffer.String()
}

// WriteLn writes a line in a given connexion
func WriteLn(conn net.Conn, content string) {
	writer := bufio.NewWriter(conn)
	_, err := writer.WriteString(content + "\n")
	if err == nil {
		err = writer.Flush()
	} else {
		log.Fatal(err)
	}
}
