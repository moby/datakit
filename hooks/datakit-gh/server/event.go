package server

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/google/go-github/github"
	"golang.org/x/net/context"

	datakit "github.com/docker/datakit/api/go-datakit"
)

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
	prDir := []string{user, repo, "prs", strconv.Itoa(*n)}
	return prDir, nil
}

func (h *Server) HandlePullRequestEvent(g GithubHeaders, e github.PullRequestEvent) error {

	// do nothing if the pull request's head didn't change
	if e.Action == nil {
		return nil
	}
	h.logger.Debugf("action: %s", *e.Action)
	switch *e.Action {
	case "opened":
	case "closed":
	case "reopened":
	case "synchronize":
	default:
		return nil
	}

	// maybe we want to move Dial in the parent function
	ctx := context.Background()
	client, err := datakit.Dial(ctx, h.proto, h.address)
	if err != nil {
		return err
	}

	// create a new transaction in the DB
	tr, err := datakit.NewTransaction(ctx, client, h.branch, h.branch+"-"+g.GitHubDelivery)
	if err != nil {
		return err
	}

	dir, err := h.PRDir(e)
	if err != nil {
		return err
	}

	if *e.Action == "closed" {
		// we only store open PRs
		tr.Remove(ctx, dir)
	} else {
		// the PR's head has changed, so store the new head
		head := e.PullRequest.Head.SHA
		if head == nil {
			return fmt.Errorf("PR %d has an invalid head", *e.Number)
		}
		tr.Write(ctx, append(dir, "head"), *head)
	}

	// commit the changes to the hook's branch
	err = tr.Commit(ctx, fmt.Sprintf("Pull request event: %s", g.GitHubDelivery))
	if err != nil {
		return err
	}

	client.Close(ctx)
	return nil
}

func (h *Server) CommitDir(e github.StatusEvent) ([]string, error) {

	if e.Repo.Owner.Login == nil {
		return nil, fmt.Errorf("Empty user")
	}
	user := *e.Repo.Owner.Login

	if e.Repo.Name == nil {
		return nil, fmt.Errorf("Empty repo")
	}
	repo := *e.Repo.Name

	if e.SHA == nil {
		return nil, fmt.Errorf("Empty SHA")
	}
	sha := *e.SHA

	h.logger.Debugf("user=%s, repo=%s", user, repo)
	commitDir := []string{user, repo, "commits", sha}
	return commitDir, nil
}

func (h *Server) HandleStatusEvent(g GithubHeaders, e github.StatusEvent) error {

	// maybe we want to move Dial in the parent function
	ctx := context.Background()
	client, err := datakit.Dial(ctx, h.proto, h.address)
	if err != nil {
		return err
	}
	defer client.Close(ctx)

	// create a new transaction in the DB
	tr, err := datakit.NewTransaction(ctx, client, h.branch, h.branch+"-"+g.GitHubDelivery)
	if err != nil {
		return err
	}

	// project the event in the the filesystem
	dir, err := h.CommitDir(e)
	if err != nil {
		return err
	}

	// the status name, e.g. 'continuous-integration/appveyor/pr'
	context := e.Context
	if context == nil {
		return fmt.Errorf("Status without a valid context")
	}
	dir = append(dir, strings.Split(*context, "/")...)

	if e.Description != nil {
		tr.Write(ctx, append(dir, "description"), *e.Description)
	}
	if e.State != nil {
		tr.Write(ctx, append(dir, "state"), *e.State)
	}
	if e.TargetURL != nil {
		tr.Write(ctx, append(dir, "target_url"), *e.TargetURL)
	}

	// commit the changes to the hook's branch
	err = tr.Commit(ctx, fmt.Sprintf("Status event: %s", g.GitHubDelivery))
	if err != nil {
		return err
	}

	return nil
}
