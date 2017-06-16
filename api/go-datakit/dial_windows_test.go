package datakit

import (
	"context"

	"github.com/Microsoft/go-winio"
)

func dial(ctx context.Context) (*Client, error) {
	conn, err := winio.DialPipe(`\\.\pipe\datakit-test`, nil)
	if err != nil {
		return nil, err
	}
	return NewClient(ctx, conn)
}
