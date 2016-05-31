package main

import (
	"fmt"
	"os"

	"github.com/Sirupsen/logrus"
	"github.com/spf13/cobra"
)

var (
	logger = logrus.New()
	config = &Config{
		Verbose: false,
		Github: GithubConfig{
			Listen: ":80",
			Secret: "",
		},
		Datakit: DatakitConfig{
			Address: "127.0.0.1:5640",
			Branch:  "github-hooks",
		},
	}
)

var cmd = &cobra.Command{
	Use:   "datakit-hg",
	Short: "datakit-hg store GitHub events into Datakit",
	Long: `datakit-gh is a small application that manage web hooks from GitHub and project
events into a Datakit instance.`,
	Run: func(cmd *cobra.Command, args []string) {
		if config.Verbose {
			logger.Level = logrus.DebugLevel
		}
		serveAction(cmd, args)
	},
}

func main() {
	if err := cmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(-1)
	}
}
