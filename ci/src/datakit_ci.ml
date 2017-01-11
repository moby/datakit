open Datakit_github

module Output = CI_output

(* FIXME: we should probably make that type abstract *)
type 'a status = 'a CI_s.status = {
  result: ('a, [`Pending of string * unit Lwt.t | `Failure of string]) result;
  output: Output.t
}

type job_id = CI_s.job_id
module Term = CI_term
include CI_main
module Utils = CI_utils
module Process = CI_process
module Live_log = CI_live_log
module Monitored_pool = CI_monitored_pool
module Cache = CI_cache
module type BUILDER = CI_s.BUILDER
module DK = Utils.DK
module ACL = CI_ACL
module Target = CI_target
module Git = CI_git

module Web = struct
  type config = CI_web_templates.t
  let config = CI_web_templates.config
end

module Private = struct
  module Client9p = Utils.Client9p
  type engine = CI_engine.t
  let connect = DK.connect
  let test_engine ~web_ui conn = CI_engine.create ~web_ui conn
  let listen = CI_engine.listen

  let create_logs = CI_live_log.create_manager
  let lookup_log = CI_live_log.lookup
  let cancel = CI_live_log.cancel
  let read_log = CI_cache.read_log
  let rebuild saved = Lazy.force saved.Output.rebuild
end

module Config = struct
  type t = CI_config.t
  type project = Repo.t * CI_config.project
  type test = CI_config.test
  let project = CI_config.project
  let v = CI_config.v
end
