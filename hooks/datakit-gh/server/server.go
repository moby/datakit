package server

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"strings"

	"github.com/Sirupsen/logrus"
	"github.com/google/go-github/github"
)

const ROUTE = "/{user:.*}/{name:.*}/"

// New returns a new http.Handler that handles github webhooks from the github API.
//
// secret is the secret provided when you register the webhook in the github UI.
// logger is the standard logger for the application
func New(address string, branch string, secret string, logger *logrus.Logger) http.Handler {
	proto := "tcp"
	addr := address
	if strings.HasPrefix(addr, "unix:") {
		proto = "unix"
		addr = addr[5:]
	}
	return &Server{
		proto:   proto,
		address: addr,
		branch:  branch,
		secret:  secret,
		logger:  logger,
	}
}

// Server handles github webhooks.
type Server struct {
	proto   string
	address string
	branch  string
	secret  string
	logger  *logrus.Logger
}

func (h *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	var (
		repo       = parseRepo(r)
		requestLog = h.logger.WithFields(newFields(r, repo))
	)

	data, err := ioutil.ReadAll(r.Body)
	r.Body.Close()
	if err != nil {
		requestLog.WithField("error", err).Error("read request body")
		http.Error(w, http.StatusText(http.StatusBadRequest), http.StatusBadRequest)
		return
	}

	if !validateSignature(requestLog, r, h.secret, data) {
		requestLog.Warn("signature verification failed")
		// return a generic NOTFOUND for auth/verification errors
		http.Error(w, http.StatusText(http.StatusNotFound), http.StatusNotFound)
		return
	}

	g := GithubHeaders{
		GitHubEvent:    r.Header.Get("X-Github-Event"),
		GitHubDelivery: r.Header.Get("X-Github-Delivery"),
		HubSignature:   r.Header.Get("X-Hub-Signature"),
	}

	requestLog.Infof("Received event: %s %s", g.GitHubEvent, g.GitHubDelivery)
	if g.GitHubEvent == "pull_request" {
		var e github.PullRequestEvent
		if err := json.Unmarshal(data, &e); err != nil {
			logrus.Error(err)
		} else if err := h.HandlePullRequestEvent(g, e); err != nil {
			logrus.Error(err)
		}
	} else if g.GitHubEvent == "status" {
		var e github.StatusEvent
		if err := json.Unmarshal(data, &e); err != nil {
			logrus.Error(err)
		} else if err := h.HandleStatusEvent(g, e); err != nil {
			logrus.Error(err)
		}
	} else {
		requestLog.Printf("Ignoring unknown event kind")
	}
}
