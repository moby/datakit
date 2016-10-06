package datakit

import (
	"bytes"
	"io"
	"log"
	"testing"

	"golang.org/x/net/context"
)

func TestInit(t *testing.T) {
	ctx := context.Background()
	log.Println("Testing the client interface")

	client, err := dial(ctx)
	if err != nil {
		t.Fatalf("Dial failed: %v", err)
	}
	err = client.Remove(ctx, "branch", "master", "rm", "does-not-exist")
	if err != nil {
		t.Fatalf("Remove failed: %v", err)
	}
	err = client.Remove(ctx, "branch", "master", "rm", "foo")
	if err != nil {
		t.Fatalf("Remove failed: %v", err)
	}
	path := []string{"branch", "master", "transactions", "foo"}
	err = client.Mkdir(ctx, path...)
	if err != nil {
		t.Fatalf("Mkdir failed: %v", err)
	}
	path = []string{"branch", "master", "transactions", "foo", "rw", "a", "b", "c"}
	err = client.Mkdir(ctx, path...)
	if err != nil {
		t.Fatalf("Mkdir failed: %v", err)
	}
	current := make([]string, len(path))
	copy(current, path)
	log.Println("Remove", current)
	for len(current) > 5 {
		err = client.Remove(ctx, current...)
		if err != nil {
			t.Fatalf("Remove %v failed: %v", current, err)
		}
		current = current[0 : len(current)-1]
	}
	err = client.Mkdir(ctx, path...)
	if err != nil {
		t.Fatalf("Mkdir failed: %v", err)
	}
	filePath := append(path, "filename")
	largeFilePath := append(path, "largefile")
	var largeDataInput []byte
	for ix := 0; ix < client.session.MaxReadSize()+150; ix++ {
		largeDataInput = append(largeDataInput, byte(ix))
	}
	err = client.Remove(ctx, filePath...)
	if err != nil {
		t.Fatalf("Remove failed: %v", err)
	}
	file, err := client.Create(ctx, filePath...)
	if err != nil {
		t.Fatalf("Create %v failed: %v", filePath, err)
	}
	message := []byte("Hello world")
	n, err := file.Write(ctx, message, 0)
	if err != nil {
		t.Fatalf("Write failed: %v", err)
	}
	sector := make([]byte, 512)
	m, err := file.Read(ctx, sector, 0)
	if err != nil {
		t.Fatalf("Read failed: %v", err)
	}
	if n != m {
		t.Fatalf("Failed to read back the number of bytes we wrote")
	}
	if string(message) != string(sector[0:m]) {
		t.Fatalf("The message we read back was different to the message we wrote")
	}
	file.Close(ctx)
	file.Close(ctx) // should be idempotent

	file, err = client.Create(ctx, largeFilePath...)
	if err != nil {
		t.Fatalf("Create %v failed: %v", filePath, err)
	}
	defer file.Close(ctx)
	n, err = file.NewIOWriter(ctx, 0).Write(largeDataInput)
	if err != nil {
		t.Fatalf("Write failed: %v", err)
	}
	if n != len(largeDataInput) {
		t.Fatalf("Write was only partial: %v", err)
	}
	readBackData := make([]byte, len(largeDataInput)+2) // make sure reported length when ReadAll is called has the right value
	n, err = io.ReadFull(file.NewIOReader(ctx, 0), readBackData)
	if err != nil && err != io.EOF && err != io.ErrUnexpectedEOF {
		t.Fatalf("Read failed: %v", err)
	}
	if n != len(largeDataInput) {
		t.Fatalf("Failed to read back the number of bytes we wrote")
	}
	if bytes.Compare(largeDataInput, readBackData[:n]) != 0 {
		t.Fatalf("The message we read back was different to the message we wrote")
	}
}
