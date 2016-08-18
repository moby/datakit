package server

import (
	"crypto/hmac"
	"crypto/sha1"
	"encoding/hex"
	"fmt"
	"net/http"

	"github.com/Sirupsen/logrus"
	"github.com/gorilla/mux"
)

type repository struct {
	User string
	Name string
}

// parseRepo returns a repository type with the user and repo's name
func parseRepo(r *http.Request) repository {
	vars := mux.Vars(r)
	return repository{
		User: vars["user"],
		Name: vars["name"],
	}
}

// validateSignature validates the request payload with the user provided key using the
// HMAC algo
func validateSignature(requestLog *logrus.Entry, r *http.Request, key string, payload []byte) bool {
	// if we don't have a secret to validate then just return true
	// because the user does not care about security
	if key == "" {
		return true
	}
	actual := r.Header.Get("X-Hub-Signature")
	expected, err := getExpectedSignature([]byte(key), payload)
	if err != nil {
		requestLog.WithField("gh_signature", actual).WithField("error", err).Error("parse expected signature")
		return false
	}
	return hmac.Equal([]byte(expected), []byte(actual))
}

// getExpectedSignature returns the expected signature for the payload by
// applying the HMAC algo with sha1 as the digest to sign the request with
// the provided key
func getExpectedSignature(key, payload []byte) (string, error) {
	mac := hmac.New(sha1.New, key)
	if _, err := mac.Write(payload); err != nil {
		return "", nil
	}
	return fmt.Sprintf("sha1=%s", hex.EncodeToString(mac.Sum(nil))), nil
}

// newFields returns the logrus.Fields for the current request so that a
// request specific logger can be constructed
func newFields(r *http.Request, repo repository) logrus.Fields {
	return logrus.Fields{
		"host": r.Host,
		"user": repo.User,
		"name": repo.Name,
	}
}
