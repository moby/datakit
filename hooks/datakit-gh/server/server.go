package server

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"strconv"
	"strings"

	"github.com/Sirupsen/logrus"
	"github.com/google/go-github/github"
	"golang.org/x/net/context"

	datakit "github.com/docker/datakit/api/go"
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

type GithubHeaders struct {
	GitHubEvent    string
	GitHubDelivery string
	HubSignature   string
}

func (h *Server) PRDir(e github.PullRequestEvent) ([]string, error) {
	n := e.Number
	if n == nil {
		return nil, fmt.Errorf("Invalid PR number")
	}

	if e.Repo.Owner.Login == nil {
		return nil, fmt.Errorf("Empty user")
	}
	user := *e.Repo.Owner.Login

	if e.Repo.Name == nil {
		return nil, fmt.Errorf("Empty repo")
	}
	repo := *e.Repo.Name

	h.logger.Debugf("user=%s, repo=%s", user, repo)
	prDir := []string{user, repo, "pr", strconv.Itoa(*n)}
	return prDir, nil
}

func (h *Server) HandlePullRequestEvent(g GithubHeaders, e github.PullRequestEvent) error {

	// maybe we want to move Dial in the parent function
	ctx := context.Background()
	client, err := datakit.Dial(ctx, h.proto, h.address)
	if err != nil {
		return err
	}

	// create a new transaction in the DB
	tr, err := datakit.NewTransaction(ctx, client, h.branch, h.branch+"-"+g.GitHubEvent)
	if err != nil {
		return err
	}

	// project the event in the the filesystem
	dir, err := h.PRDir(e)
	if err != nil {
		return err
	}

	// store the head's SHA1
	head := e.PullRequest.Head.SHA
	if head == nil {
		return fmt.Errorf("PR %d has an invalid head", *e.Number)
	}
	tr.Write(ctx, append(dir, "head"), *head)

	// fetch the remote contents
	// TODO

	// commit the changes to the hook's branch
	err = tr.Commit(ctx)
	if err != nil {
		return err
	}

	client.Close(ctx)
	return nil
}

func (h *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	var (
		repo       = parseRepo(r)
		requestLog = h.logger.WithFields(newFields(r, repo))
	)
	requestLog.Debug("web hook received")

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

	requestLog.Infof("Received event: %s", g.GitHubEvent)
	if g.GitHubEvent == "pull_request" {
		var e github.PullRequestEvent
		if err := json.Unmarshal(data, &e); err != nil {
			logrus.Error(err)
		} else if err := h.HandlePullRequestEvent(g, e); err != nil {
			logrus.Error(err)
		}
	} else {
		requestLog.Printf("Ignoring unknown event kind")
	}
}
