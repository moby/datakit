// +build linux darwin

package datakit

import "context"

func dial(ctx context.Context) (*Client, error) {
	return Dial(ctx, "unix", "/var/tmp/foo")
}
