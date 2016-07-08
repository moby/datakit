package main

import (
	"net/http"

	"github.com/docker/datakit/hooks/datakit-gh-hooks/server"
	"github.com/gorilla/mux"
	"github.com/spf13/cobra"
)

func serveAction(cmd *cobra.Command, args []string) {
	r := mux.NewRouter()
	s := server.New(config.Datakit.Address, config.Datakit.Branch, config.Github.Secret, logger)
	r.Handle(server.ROUTE, s).Methods("POST")
	logger.Debugf("Listening to %s", config.Github.Listen)
	if err := http.ListenAndServe(config.Github.Listen, r); err != nil {
		logger.Fatal(err)
	}
}
