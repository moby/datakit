module Step_log = CI_result.Step_log

type 'a lwt_status = 'a CI_s.lwt_status

module Term = CI_term
module Main = CI_main
module Utils = CI_utils
module Process = CI_process
module Live_log = CI_live_log
module Monitored_pool = CI_monitored_pool
module ProjectID = CI_projectID
module Github_hooks = CI_github_hooks
module Cache = CI_cache
module type BUILDER = CI_s.BUILDER
module DK = Utils.DK

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
end
