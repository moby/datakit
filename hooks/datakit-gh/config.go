package main

import "time"

type duration struct {
	time.Duration
}

func (d *duration) UnmarshalText(text []byte) (err error) {
	d.Duration, err = time.ParseDuration(string(text))
	return err
}

type GithubConfig struct {
	Listen string
	Secret string
}

type DatakitConfig struct {
	Address string
	Branch  string
}

type Config struct {
	Verbose bool
	Github  GithubConfig
	Datakit DatakitConfig
}

func initConfig() {

}

func init() {
	cmd.Flags().BoolVarP(&config.Verbose, "verbose", "v", config.Verbose, "Verbose output")
	cmd.Flags().StringVarP(&config.Github.Listen, "listen", "l", config.Github.Listen, "Address to listen to webhooks")
	cmd.Flags().StringVarP(&config.Github.Secret, "secret", "s", config.Github.Secret, "Webhooks secret")
	cmd.Flags().StringVarP(&config.Datakit.Address, "address", "a", config.Datakit.Address, "Address of datakit instance")
	cmd.Flags().StringVarP(&config.Datakit.Branch, "branch", "b", config.Datakit.Branch, "Datakit branch to commit to")
}
