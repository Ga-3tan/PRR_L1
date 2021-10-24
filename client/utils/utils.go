// Package utils contains util functions used in the program
package utils

import (
	"bufio"
	"bytes"
	"io"
	"net"
)

// ReadLn is used to read a line from a source connexion
func ReadLn(conn net.Conn) (string, error) {
	reader := bufio.NewReader(conn)
	var buffer bytes.Buffer
	for {
		ba, isPrefix, err := reader.ReadLine()
		if err != nil {
			if err == io.EOF {
				break
			}
			return "", err
		}
		buffer.Write(ba)
		if !isPrefix {
			break
		}
	}
	return buffer.String(), nil
}

// WriteLn writes a line in a given connexion
func WriteLn(conn net.Conn, content string) (int, error) {
	writer := bufio.NewWriter(conn)
	number, err := writer.WriteString(content + "\n")
	if err == nil {
		err = writer.Flush()
	}
	return number, err
}